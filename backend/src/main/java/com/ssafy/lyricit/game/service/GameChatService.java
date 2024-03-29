package com.ssafy.lyricit.game.service;

import static com.ssafy.lyricit.common.type.EventType.*;

import java.util.List;

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

	@Value("${ELASTICSEARCH_URL}")
	private String url;

	public void checkGameChatMessage(GameChatDto chatRequest) {
		// 유효성 검증
		// 1. 해당 방에 게임이 진행 중인지
		// 2. 해당 방에 해당 유저가 있는게 맞는지
		// 3. 메시지가 비어있거나 공백만 포함하는 경우는 아닌지

		// request 변수
		String roomNumber = chatRequest.getRoomNumber();
		String memberId = chatRequest.getMemberId();
		String content = chatRequest.getContent();

		// 게임 정보 불러오기
		GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// highlight 상태인지 확인
		if (game.getHighlightInfo().getMemberId().equals("")) {
			checkLyric(roomNumber, memberId, content);
		} else {
			// 해당 메시지를 보낸 유저가 highlight 멤버인지 확인
			if (!game.getHighlightInfo().getMemberId().equals(memberId)) {
				// highlight 멤버가 아닌 경우, 그냥 메시지 전달
				sendGameChatMessage(roomNumber, memberId, content);
			} else {
				if (game.getHighlightInfo().getTitle().equals("")) {
					// 제목이 아직 비어있는 경우
					checkTitle(roomNumber, memberId, content);
				} else {
					// 제목이 이미 있는 경우, 가수 입력
					checkSinger(roomNumber, memberId, content);
				}
			}
		}
	}

	private void checkLyric(String roomNumber, String memberId, String content) {

		// ElasticSearch 검색
		String requestBody = "{ \"query\": { \"match_phrase\": { \"lyrics\": \"" + content + "\" } } }";

		ElasticSearchResponseDto response = webClient.post()
			.uri(url)
			.header("Authorization", "ApiKey ${ELASTICSEARCH_API_KEY}")
			.bodyValue(requestBody)
			.retrieve()
			.bodyToMono(ElasticSearchResponseDto.class)
			.block();

		if (response.getHits().getTotal().getValue() == 0) {
			// 정답이 없는 경우 그냥 메세지 처리
			sendGameChatMessage(roomNumber, memberId, content);
		} else if (response.getHits().getTotal().getValue() > 0) {
			// 게임 불러오기
			GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

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

			// 하이라이트 상태를 방에 전달
			HighlightNoticeDto notice = highlightInfo.toHighlightNoticeDto();
			messagePublisher.publishGameToRoom(HIGHLIGHT.name(), roomNumber, notice);

		}
	}

	private void checkTitle(String roomNumber, String memberId, String content) {

		// 게임 불러오기
		GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 검색할 변수
		String lyric = game.getHighlightInfo().getLyric();
		String title = content.replace(" ", "");

		// ElasticSearch 검색
		String requestBody = "{ \"query\" : { \"bool\" : { \"must\" : [ {\"match\" : { \"title\" : \"" + title + "\" }}, { \"match_phrase\": { \"lyrics\" : \"" + lyric + "\" }} ] } } }";

		ElasticSearchResponseDto response = webClient.post()
			.uri(url)
			.header("Authorization", "ApiKey ${ELASTICSEARCH_API_KEY}")
			.bodyValue(requestBody)
			.retrieve()
			.bodyToMono(ElasticSearchResponseDto.class)
			.block();

		// 검색결과 있으면 제목 맞음 알림
		if (response.getHits().getTotal().getValue() > 0) {

			// 제목 정보 redis에 갱신
			HighlightDto highlightInfo = game.getHighlightInfo();
			highlightInfo = highlightInfo.toBuilder()
				.title(title)
				.build();

			game = game.toBuilder()
				.highlightInfo(highlightInfo)
				.build();

			gameRedisTemplate.opsForValue().set(roomNumber, game);

			// 제목 정답 알림
			messagePublisher.publishGameToRoom(CORRECT_TITLE.name(), roomNumber, title);
		} else {
			// 검색결과 없으면 오답처리
			handleIncorrectAnswer(roomNumber, memberId);
		}
	}

	private void checkSinger(String roomNumber, String memberId, String content) {
		// 게임 불러오기
		GameDto game = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 검색할 변수
		String lyric = game.getHighlightInfo().getLyric();
		String title = game.getHighlightInfo().getTitle();

		// ElasticSearch 검색
		String requestBody = "{ \"query\" : { \"bool\" : { \"must\" : [ {\"match\" : { \"title\" : \"" + lyric + "\" }}, {\"match\" : { \"artist\" : \"" + content + "\" }}, { \"match_phrase\": { \"lyrics\" : \"" + title + "\" }} ] } } }";

		ElasticSearchResponseDto response = webClient.post()
			.uri(url)
			.header("Authorization", "ApiKey ${ELASTICSEARCH_API_KEY}")
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



	private void sendGameChatMessage(String roomNumber, String nickname, String content) {
		GameChatResponseDto response = GameChatResponseDto.builder()
			.nickname(nickname)
			.content(content)
			.build();

		messagePublisher.publishMessageToGame(roomNumber, response);
	}

	private void handleIncorrectAnswer(String roomNumber, String memberId) {
		// 게임 정보 갱신 (하이라이트 정보 초기화)
		GameDto gameDto = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);
		HighlightDto highlightInfo = gameDto.getHighlightInfo();
		highlightInfo = highlightInfo.toBuilder()
			.memberId("")
			.lyric("")
			.title("")
			.build();
		gameDto = gameDto.toBuilder()
			.highlightInfo(highlightInfo)
			.build();
		gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// 해당 멤버 정보
		RoomDto room = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber);
		MemberDto member = room.getMembers().stream()
			.filter(memberInGameDto -> memberInGameDto.getMember().memberId().equals(memberId))
			.findFirst()
			.get()
			.getMember();

		// 오답 알림 pub
		messagePublisher.publishGameToRoom(INCORRECT_ANSWER.name(), roomNumber, member);
	}

	private void handleCorrectAnswer(String roomNumber, String memberId) {
		GameDto gameDto = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 해당 멤버 정보
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

		// redis 게임 정보 갱신
		// 정답 맞춘 해당 멤버 점수 갱신
		gameDto.getMembers().stream()
			.filter(scoreDto -> scoreDto.getMemberId().equals(memberId))
			.findFirst()
			.get()
			.setScore(totalScore + 1000L);

		HighlightDto highlightInfo = gameDto.getHighlightInfo();
		highlightInfo = highlightInfo.toBuilder()
			.memberId("")
			.lyric("")
			.title("")
			.build();
		List<String> correctMembers = gameDto.getCorrectMembers();
		correctMembers.add(memberId);

		gameDto = gameDto.toBuilder()
			.highlightInfo(highlightInfo)
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
		}
	}
}
