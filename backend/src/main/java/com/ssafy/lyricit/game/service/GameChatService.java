package com.ssafy.lyricit.game.service;

import static com.ssafy.lyricit.common.type.EventType.*;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;

import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.game.dto.CorrectAnswerDto;
import com.ssafy.lyricit.game.dto.ElasticSearchResponseDto;
import com.ssafy.lyricit.game.dto.GameChatDto;
import com.ssafy.lyricit.game.dto.GameChatResponseDto;
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.game.dto.HighlightDto;
import com.ssafy.lyricit.game.dto.HighlightNoticeDto;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.room.dto.RoomDto;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class GameChatService {
	private final MessagePublisher messagePublisher;
	private final RedisTemplate<String, Object> roomRedisTemplate;
	private final RedisTemplate<String, Object> gameRedisTemplate;
	private final GameService gameService;
	private final WebClient webClient = WebClient.builder().build();
	private final Logger log = LoggerFactory.getLogger(this.getClass());

	private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(10);
	private final ConcurrentHashMap<String, ScheduledFuture<?>> highlightTasks = new ConcurrentHashMap<>();


	@Value("${ELASTICSEARCH_URL}")
	private String url;

	@Value("${ELASTICSEARCH_API_KEY}")
	private String apiKey;

	public void checkGameChatMessage(GameChatDto chatRequest) {
		// request 변수
		String roomNumber = chatRequest.getRoomNumber();
		String memberId = chatRequest.getMemberId();

		// 게임 정보 불러오기
		GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// highlight 상태인지 확인
		if (game.getHighlightInfo().getMemberId().equals("")) {
			handleLyric(chatRequest);
		} else {
			// 해당 메시지를 보낸 유저가 highlight 멤버인지 확인
			if (!game.getHighlightInfo().getMemberId().equals(memberId)) {
				// highlight 멤버가 아닌 경우, 그냥 메시지 전달
				sendGameChatMessage(chatRequest);
			} else {
				if (game.getHighlightInfo().getTitle().equals("")) {
					// 제목이 아직 비어있는 경우
					handleTitle(chatRequest);
				} else {
					// 제목이 이미 있는 경우, 정답 체크
					checkAnswer(chatRequest);
				}
			}
		}
	}

	private void handleLyric(GameChatDto chatRequest) {
		String roomNumber = chatRequest.getRoomNumber();
		String memberId = chatRequest.getMemberId();
		String content = chatRequest.getContent();
		GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// content 에 keyword가 포함되어 있지 않다면 아래 로직 실행하지 않고 그냥 메시지 전달
		if (!chatRequest.getContent().contains(game.getKeyword())) {
			sendGameChatMessage(chatRequest);
			return;
		}

		// ElasticSearch 검색
		String requestBody =
			"{ \"query\": { \"match_phrase\": { \"lyrics\": \"" + chatRequest.getContent() + "\" } } }";

		ElasticSearchResponseDto response = webClient.post()
			.uri(url)
			.header("Authorization", "ApiKey " + apiKey)
			.header("Content-Type", "application/json")
			.bodyValue(requestBody)
			.retrieve()
			.bodyToMono(ElasticSearchResponseDto.class)
			.block();

		if (response.getHits().getTotal().getValue() == 0) {
			// 정답이 없는 경우 그냥 메세지 처리
			sendGameChatMessage(chatRequest);
		} else if (response.getHits().getTotal().getValue() > 0) {
			// 정답이 있는 경우는 하이라이트 상태로 전환
			HighlightDto highlightInfo = game.getHighlightInfo();
			highlightInfo = highlightInfo.toBuilder()
				.memberId(memberId)
				.lyric(content)
				.build();
			game = game.toBuilder()
				.highlightInfo(highlightInfo)
				.build();
			gameRedisTemplate.opsForValue().set(roomNumber, game);
			GameDto gameDto = game;

			// 하이라이트 상태를 방에 전달
			HighlightNoticeDto highlightNoticeDto = highlightInfo.toHighlightNoticeDto();
			messagePublisher.publishGameToRoom(HIGHLIGHT.name(), roomNumber, highlightNoticeDto);

			// 하이라이트 시간제한 스케줄링
			ScheduledFuture<?> highlightTask = scheduler.schedule(() -> handleIncorrectAnswer(roomNumber, memberId), 15L,
				TimeUnit.SECONDS);

			highlightTasks.put(roomNumber, highlightTask);
		}
	}

	private void handleTitle(GameChatDto chatRequest) {

		String roomNumber = chatRequest.getRoomNumber();

		// 게임 불러오기
		GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 입력받은 제목 정보 redis에 갱신
		String title = chatRequest.getContent().replace(" ", "");
		HighlightDto highlightInfo = game.getHighlightInfo();
		highlightInfo = highlightInfo.toBuilder()
			.title(title)
			.build();

		game = game.toBuilder()
			.highlightInfo(highlightInfo)
			.build();

		gameRedisTemplate.opsForValue().set(roomNumber, game);

		// 제목 알림 pub
		messagePublisher.publishGameToRoom(HIGHLIGHT_TITLE.name(), roomNumber, chatRequest.getContent());
	}

	private void checkAnswer(GameChatDto chatRequest) {

		String roomNumber = chatRequest.getRoomNumber();
		String memberId = chatRequest.getMemberId();
		// 게임 불러오기
		GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 검색할 변수
		String lyric = game.getHighlightInfo().getLyric();
		String title = game.getHighlightInfo().getTitle();

		// ElasticSearch 검색
		String requestBody = "{ \"query\" : { \"bool\" : { \"must\" : [ {\"match\" : { \"title\" : \"" + lyric
			+ "\" }}, {\"match\" : { \"artist\" : \"" + chatRequest.getContent()
			+ "\" }}, { \"match_phrase\": { \"lyrics\" : \"" + title + "\" }} ] } } }";

		ElasticSearchResponseDto response = webClient.post()
			.uri(url)
			.header("Authorization", "ApiKey " + apiKey)
			.header("Content-Type", "application/json")
			.bodyValue(requestBody)
			.retrieve()
			.bodyToMono(ElasticSearchResponseDto.class)
			.block();

		if (response.getHits().getTotal().getValue() == 0) {
			// 검색결과 없으면 오답처리
			handleIncorrectAnswer(roomNumber, memberId);
		} else {
			// 검색결과 있으면 정답처리
			handleCorrectAnswer(roomNumber, memberId);
		}
	}

	private void sendGameChatMessage(GameChatDto chatRequest) {
		GameChatResponseDto response = chatRequest.toGameChatResponseDto();

		messagePublisher.publishMessageToGame(chatRequest.getRoomNumber(), response);
	}

	private void handleIncorrectAnswer(String roomNumber, String memberId) {
		// 하이라이트 시간제한 스케줄링 취소
		cancelHighlightTask(roomNumber);
		log.info("roomNumber : " + roomNumber);

		// 해당 멤버 정보
		RoomDto room = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber);
		log.info("1");
		MemberDto member = room.getMembers().stream()
			.filter(memberInGameDto -> memberInGameDto.getMember().memberId().equals(memberId))
			.findFirst()
			.get()
			.getMember();

		// 오답 알림 pub
		messagePublisher.publishGameToRoom(INCORRECT_ANSWER.name(), roomNumber, member);

		// 2초 후 하이라이트 취소 메서드 호출
		scheduler.schedule(() -> cancelHighlight(roomNumber), 2, TimeUnit.SECONDS);
	}

	private void handleCorrectAnswer(String roomNumber, String memberId) {
		// 하이라이트 시간제한 스케줄링 취소
		cancelHighlightTask(roomNumber);

		// 해당 게임 및 멤버 정보
		GameDto gameDto = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);
		RoomDto room = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber);
		MemberDto member = room.getMembers().stream()
			.filter(memberInGameDto -> memberInGameDto.getMember().memberId().equals(memberId))
			.findFirst()
			.get()
			.getMember();
		Long totalScore = gameDto.getMembers().stream()
			.filter(scoreDto -> scoreDto.getMemberId().equals(memberId))
			.findFirst()
			.get()
			.getScore();

		// redis 에 게임 정보 갱신
		// 정답 맞춘 해당 멤버 점수 갱신
		gameDto.getMembers().stream()
			.filter(scoreDto -> scoreDto.getMemberId().equals(memberId))
			.findFirst()
			.get()
			.setScore(totalScore + 1000L);
		// 정답자 목록에 추가
		List<String> correctMembers = gameDto.getCorrectMembers();
		correctMembers.add(memberId);

		gameDto = gameDto.toBuilder()
			.correctMembers(correctMembers)
			.build();
		gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// 정답 알림 pub
		CorrectAnswerDto correctAnswerDto = CorrectAnswerDto.builder()
			.member(member)
			.score(1000L)
			.totalScore(totalScore + 1000L)
			.build();

		messagePublisher.publishGameToRoom(CORRECT_ANSWER.name(), roomNumber, correctAnswerDto);

		// 해당 방의 전원이 정답을 맞추었다면 다음 라운드로 넘어가기
		if (gameDto.getCorrectMembers().size() == gameDto.getPlayerCount()) {
			gameService.startRound(roomNumber);
		} else {
			// 2초 후 하이라이트 취소 메서드 호출
			scheduler.schedule(() -> cancelHighlight(roomNumber), 2, TimeUnit.SECONDS);
		}

	}

	public void cancelHighlightTask(String roomNumber) {
		ScheduledFuture<?> oldTask = highlightTasks.get(roomNumber);
		if (oldTask != null && !oldTask.isCancelled()) {
			oldTask.cancel(true);
		}
	}

	private void cancelHighlight(String roomNumber) {
		// 게임 불러오기
		GameDto gameDto = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 게임 정보 갱신 (하이라이트 정보 초기화)
		HighlightDto highlightDto = gameDto.getHighlightInfo();
		highlightDto = highlightDto.toBuilder()
			.memberId("")
			.lyric("")
			.title("")
			.build();
		gameDto = gameDto.toBuilder()
			.highlightInfo(highlightDto)
			.build();

		gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// 메세지 pub
		messagePublisher.publishGameToRoom(HIGHLIGHT_CANCELLED.name(), roomNumber);
	}
}
