package com.ssafy.lyricit.game.service;

import static com.ssafy.lyricit.common.type.EventType.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.quartz.SchedulerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.chat.dto.RoomChatRequestDto;
import com.ssafy.lyricit.chat.dto.RoomChatResponseDto;
import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.game.constant.ScoreValue;
import com.ssafy.lyricit.game.dto.CorrectAnswerDto;
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.game.dto.HighlightDto;
import com.ssafy.lyricit.game.dto.HighlightNoticeDto;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.service.RoomService;
import com.ssafy.lyricit.round.service.RoundService;
import com.ssafy.lyricit.search.dto.ElasticSearchResponseDto;
import com.ssafy.lyricit.search.service.SearchService;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class GameChatService {

	private final MessagePublisher messagePublisher;
	private final RedisTemplate<String, RoomDto> roomRedisTemplate;
	private final RedisTemplate<String, GameDto> gameRedisTemplate;
	private final RoomService roomService;
	private final RoundService roundService;
	private final SearchService searchService;

	private final Logger log = LoggerFactory.getLogger(this.getClass());

	private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(50);
	private final ConcurrentHashMap<String, ScheduledFuture<?>> highlightTasks = new ConcurrentHashMap<>();

	// 게임 내 채팅 메세지 확인하는 메서드
	// 먼저 해당 방이 하이라이트 상태인지 확인
	// 만약 해당 방이 하이라이트 상태가 아니라면 handleLyric 메서드 호출
	// 해당 방이 하이라이트 상태라면, 해당 메시지를 보낸 유저가 하이라이트 대상 멤버인지 확인
	// 하이라이트 대상 멤버가 아니라면 그냥 채팅방에 뿌리기
	// 하이라이트 대상 멤버라면, 제목을 아직 치지 않은 경우 handleTitle 메서드를, 가수만 남은 상태라면 checkAnswer 메서드를 호출
	public void checkGameChatMessage(RoomChatRequestDto chatRequest) throws SchedulerException {
		// request 변수
		String roomNumber = chatRequest.roomNumber();
		String memberId = chatRequest.memberId();

		// 게임 정보 불러오기
		GameDto game = roundService.validateGame(roomNumber);

		// 이미 게임을 맞춘 유저라면 그냥 메시지 전달
		if (game.getCorrectMembers().contains(memberId)) {
			sendGameChatMessage(chatRequest);
			return;
		}

		// highlight 상태인지 확인
		if (game.getHighlightDto().getMemberId().isEmpty()) {
			handleLyric(chatRequest, game);
			return;
		}
		// 해당 메시지를 보낸 유저가 highlight 멤버인지 확인
		if (!game.getHighlightDto().getMemberId().equals(memberId)) {
			// highlight 멤버가 아닌 경우, 그냥 메시지 전달
			sendGameChatMessage(chatRequest);
			return;
		}
		if (game.getHighlightDto().getTitle().isEmpty()) {
			// 제목이 아직 비어있는 경우
			handleTitle(chatRequest, game);
			return;
		}
		// 제목이 이미 있는 경우, 정답 체크
		checkAnswer(chatRequest, game);
	}

	// 해당 채팅 메세지가 하이라이트 대상인지 확인하는 메서드
	// 전달받은 채팅을 Elastic Search 에 검색하여,
	// 검색결과가 아예 없는 경우에는 그냥 채팅 메세지로 처리하고,
	// 검색결과가 나오는 경우에는, 하이라이트 상태로 전환하게 됨
	private void handleLyric(RoomChatRequestDto chatRequest, GameDto game) {
		String roomNumber = chatRequest.roomNumber();
		String memberId = chatRequest.memberId();
		String content = chatRequest.content();

		// content 에 keyword 가 포함되어 있지 않다면 아래 로직 실행하지 않고 그냥 메시지 전달
		if (!chatRequest.content().contains(game.getKeyword())) {
			sendGameChatMessage(chatRequest);
			return;
		}

		// 입력받은 채팅메세지가 8자 미만인 경우에는 바로 채팅메시지로 전달
		if (content.trim().length() < 8) {
			sendGameChatMessage(chatRequest);
			return;
		}

		// 엘라스틱 서치에 검색하여 결과값 받아오기
		ElasticSearchResponseDto response = searchService.searchLyrics(content);

		assert response != null;
		if (response.getHits().getTotal().getValue() == 0) {
			// 정답이 없는 경우 그냥 메세지 처리
			sendGameChatMessage(chatRequest);
		} else if (response.getHits().getTotal().getValue() > 0) {
			// 정답이 있는 경우 채팅메세지 뿌리고 하이라이트로 전환
			sendGameChatMessage(chatRequest);

			HighlightDto highlightInfo = game.getHighlightDto();
			highlightInfo = highlightInfo.toBuilder()
				.memberId(memberId)
				.lyric(content)
				.build();
			game = game.toBuilder()
				.highlightDto(highlightInfo)
				.build();
			gameRedisTemplate.opsForValue().set(roomNumber, game);

			// 하이라이트 상태를 방에 전달
			HighlightNoticeDto highlightNoticeDto = highlightInfo.toHighlightNoticeDto();
			messagePublisher.publishGameToRoom(HIGHLIGHT.name(), roomNumber, highlightNoticeDto);

			RoomDto room = roomService.validateRoom(roomNumber);
			// 하이라이트 시간제한 스케줄링 (15초 이내에 정답을 맞추지 못하면 오답처리)
			ScheduledFuture<?> highlightTask = scheduler.schedule(
				() -> handleTimeOver(roomNumber, memberId, room),
				15L,
				TimeUnit.SECONDS);

			highlightTasks.put(roomNumber, highlightTask);
		}
	}

	// 하이라이트 상태에서 제목을 입력받았을 때 실행되는 메서드
	// 해당 제목을 redis 에 갱신하고, 입력받았던 제목을 방에 pub
	private void handleTitle(RoomChatRequestDto chatRequest, GameDto game) {

		// 채팅메세지 전달
		sendGameChatMessage(chatRequest);

		String roomNumber = chatRequest.roomNumber();

		// 입력받은 제목 정보 redis 에 갱신
		String title = chatRequest.content().replace(" ", "");
		HighlightDto highlightInfo = game.getHighlightDto();
		highlightInfo = highlightInfo.toBuilder()
			.title(title)
			.build();

		game = game.toBuilder()
			.highlightDto(highlightInfo)
			.build();

		gameRedisTemplate.opsForValue().set(roomNumber, game);

		// 제목 알림 pub
		messagePublisher.publishGameToRoom(HIGHLIGHT_TITLE.name(), roomNumber, chatRequest.content());
	}

	// 최종적으로 정답인지 확인하는 메서드
	// 가사 + 제목 + 가수 가 일치하는 곡이 있는 지 확인 -> Elastic Search 에서 검색
	// 검색결과가 없으면 오답처리, 있으면 정답처리
	private void checkAnswer(RoomChatRequestDto chatRequest, GameDto game) throws SchedulerException {

		// 채팅메세지 전달
		sendGameChatMessage(chatRequest);

		String roomNumber = chatRequest.roomNumber();
		String memberId = chatRequest.memberId();

		// 입력받은 가수정보 먼저 방에 뿌려주기
		messagePublisher.publishGameToRoom(HIGHLIGHT_ARTIST.name(), roomNumber, chatRequest.content());

		// 검색할 변수
		String lyric = game.getHighlightDto().getLyric();
		String title = game.getHighlightDto().getTitle();

		// 엘라스틱 서치에 검색하기
		ElasticSearchResponseDto response = searchService.searchAnswer(lyric, title, chatRequest.content());

		RoomDto room = roomService.validateRoom(roomNumber);

		assert response != null;
		if (response.getHits().getTotal().getValue() == 0) {
			// 검색결과 없으면 오답처리
			handleIncorrectAnswer(roomNumber, memberId, room);
		} else {
			// 이미 정답 목록에 있는 곡인 경우
			if (game.getAnswerTracks().contains(response.getHits().getHits()[0].get_id())) {
				handleIncorrectAnswer(roomNumber, memberId, room);
				return;
			}
			// 검색결과 있으면 정답처리
			handleCorrectAnswer(roomNumber, memberId, room, game, response);
		}
	}

	// 방에 게임 채팅 뿌리는 메서드
	private void sendGameChatMessage(RoomChatRequestDto chatRequest) {
		RoomChatResponseDto response = chatRequest.toResponseDto();

		messagePublisher.publishMessageToGame(chatRequest.roomNumber(), response);
	}

	// Highlioght 시간제한이 모두 지나면 실행
	private void handleTimeOver(String roomNumber, String memberId, RoomDto room) {
		// 하이라이트 시간제한 스케줄링 취소
		cancelHighlightTask(roomNumber);
		log.info("roomNumber : " + roomNumber);

		// 해당 멤버 정보
		MemberDto member = room.getMembers().stream()
			.filter(memberInGameDto -> memberInGameDto.getMember().memberId().equals(memberId))
			.findFirst()
			.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND))
			.getMember();

		// 오답 알림 pub
		messagePublisher.publishGameToRoom(INCORRECT_ANSWER.name(), roomNumber, member);

		// 2초 후 하이라이트 취소 메서드 호출
		scheduler.schedule(() -> cancelHighlight(roomNumber), 2, TimeUnit.SECONDS);
	}

	// 오답 처리 진행하는 메서드
	// 방에 오답 알림 pub 하고 2초 후 하이라이트 취소 메서드 호출
	private void handleIncorrectAnswer(String roomNumber, String memberId, RoomDto room) {
		// 하이라이트 시간제한 스케줄링 취소
		cancelHighlightTask(roomNumber);
		log.info("roomNumber : " + roomNumber);

		// 해당 멤버 정보
		log.info("1");
		MemberDto member = room.getMembers().stream()
			.filter(memberInGameDto -> memberInGameDto.getMember().memberId().equals(memberId))
			.findFirst()
			.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND))
			.getMember();

		// 2초 뒤 오답 알림 pub
		scheduler.schedule(() -> messagePublisher.publishGameToRoom(INCORRECT_ANSWER.name(), roomNumber, member), 2,
			TimeUnit.SECONDS);

		// 2초 후 하이라이트 취소 메서드 호출
		scheduler.schedule(() -> cancelHighlight(roomNumber), 4, TimeUnit.SECONDS);
	}

	// 정답 맟췄을 시 진행하는 메서드
	// 해당 멤버 점수정보 및 정답자 목록을 갱신하고, 정답알림을 방에 pub 함
	// 만약, 해당 방에 모든 사람이 정답을 맞췄다면, 다음 라운드로 넘어가는 메서드 호출
	// 그렇지 않다면, 2초 후 하이라이트 취소 메서드 호출
	private void handleCorrectAnswer(String roomNumber, String memberId, RoomDto room, GameDto game,
		ElasticSearchResponseDto response) throws
		SchedulerException {
		// 하이라이트 시간제한 스케줄링 취소
		cancelHighlightTask(roomNumber);

		// 해당 게임 및 멤버 정보
		MemberDto member = room.getMembers().stream()
			.filter(memberInGameDto -> memberInGameDto.getMember().memberId().equals(memberId))
			.findFirst()
			.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND))
			.getMember();
		Long totalScore = game.getMembers().stream()
			.filter(scoreDto -> scoreDto.getMemberId().equals(memberId))
			.findFirst()
			.orElseThrow(() -> new BaseException(SCORE_NOT_FOUND))
			.getScore();

		// redis 에 게임 정보 갱신
		List<String> correctMembers = game.getCorrectMembers();
		List<String> answerTracks = game.getAnswerTracks();
		Long addedScore = ScoreValue.values()[correctMembers.size()].getValue();

		game.getMembers().stream()
			.filter(scoreDto -> scoreDto.getMemberId().equals(memberId))
			.findFirst()
			.orElseThrow(() -> new BaseException(SCORE_NOT_FOUND))
			.setScore(totalScore + addedScore);
		correctMembers.add(memberId);
		answerTracks.add(response.getHits().getHits()[0].get_id());

		game = game.toBuilder()
			.correctMembers(correctMembers)
			.answerTracks(answerTracks)
			.build();
		gameRedisTemplate.opsForValue().set(roomNumber, game);

		// 정답 알림 pub
		CorrectAnswerDto correctAnswerDto = CorrectAnswerDto.builder()
			.member(member)
			.score(addedScore)
			.totalScore(totalScore + addedScore)
			.answerTitle(response.getHits().getHits()[0].get_source().getTitle())
			.answerArtist(response.getHits().getHits()[0].get_source().getArtist())
			.build();

		// 2초 뒤 정답 알림
		scheduler.schedule(
			() -> messagePublisher.publishGameToRoom(CORRECT_ANSWER.name(), roomNumber, correctAnswerDto), 2,
			TimeUnit.SECONDS);

		// 해당 방의 전원이 정답을 맞추었다면 다음 라운드로 넘어가기
		if (game.getCorrectMembers().size() == game.getPlayerCount()) {
			scheduler.schedule(() -> cancelHighlight(roomNumber), 4, TimeUnit.SECONDS);
			roundService.cancelScheduledJobs(roomNumber, false);
		} else {
			// 2초 후 하이라이트 취소 메서드 호출
			scheduler.schedule(() -> cancelHighlight(roomNumber), 4, TimeUnit.SECONDS);
		}

	}

	// 하이라이트 시간제한 스케줄링을 취소하는 메서드
	public void cancelHighlightTask(String roomNumber) {
		ScheduledFuture<?> oldTask = highlightTasks.get(roomNumber);
		if (oldTask != null && !oldTask.isCancelled()) {
			oldTask.cancel(true);
		}
	}

	// 하이라이트 상태 취소하는 메서드
	// 하이라이트 정보를 초기화 하고 방에 알림 pub
	private void cancelHighlight(String roomNumber) {
		// 게임 불러오기
		GameDto gameDto = roundService.validateGame(roomNumber);

		// 게임 정보 갱신 (하이라이트 정보 초기화)
		HighlightDto highlightDto = gameDto.getHighlightDto();
		highlightDto = highlightDto.toBuilder()
			.memberId("")
			.lyric("")
			.title("")
			.build();
		gameDto = gameDto.toBuilder()
			.highlightDto(highlightDto)
			.build();

		gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// 메세지 pub
		messagePublisher.publishGameToRoom(HIGHLIGHT_CANCELLED.name(), roomNumber);
	}
}
