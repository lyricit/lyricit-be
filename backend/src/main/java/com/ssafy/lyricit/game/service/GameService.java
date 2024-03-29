package com.ssafy.lyricit.game.service;

import static com.ssafy.lyricit.common.type.EventType.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.game.domain.Keyword;
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.game.dto.GameRoundDto;
import com.ssafy.lyricit.game.repository.KeywordRepository;
import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.dto.RoomOutsideDto;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class GameService {
	private final RedisTemplate<String, Object> roomRedisTemplate;
	private final RedisTemplate<String, Object> gameRedisTemplate;
	private final MessagePublisher messagePublisher;
	private final KeywordRepository keywordRepository;

	// 라운드 스케줄링을 위한 ExecutorService
	private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
	private final ConcurrentHashMap<String, ScheduledFuture<?>> roundTasks = new ConcurrentHashMap<>();

	private final Logger log = LoggerFactory.getLogger(this.getClass());

	public void startGame(String memberId, String roomNumber) {
		// 유효한 방인지 확인
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(ROOM_NOT_FOUND);
		}
		// 게임이 이미 진행 중인지 확인
		if (Boolean.TRUE.equals(gameRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(GAME_ALREADY_PLAYING);
		}

		// 해당 방의 정보 가져오기
		RoomDto roomDto = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber);

		// 방장이 보낸 요청인지 확인
		if (Boolean.FALSE.equals(memberId.equals(roomDto.getLeaderId()))) {
			throw new BaseException(NOT_LEADER);
		}
		// 방의 전원이 모두 레디 상태인지 확인
		if (Boolean.FALSE.equals(checkAllReady(roomDto))) {
			throw new BaseException(NOT_ALL_READY);
		}

		// room의 isPlaying 정보 갱신
		roomDto.setIsPlaying(true);
		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);

		// redis 에 저장
		GameDto gameDto = GameDto.builder()
			.playerCount(roomDto.getPlayerCount())
			.roundTime(roomDto.getRoundTime())
			.roundLimit(roomDto.getRoundLimit())
			.currentRound(0L)
			.keyword("")
			.answerCount(0L)
			.members(roomDto.getMembers().stream()
				.map(MemberInGameDto::toScoreDto)
				.toList())
			.build();

		gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// pub to lounge
		RoomOutsideDto roomOutsideDto = roomDto.toOutsideDto(roomNumber);
		messagePublisher.publishRoomToLounge(ROOM_UPDATED.name(), roomOutsideDto);

		// pub to room
		messagePublisher.publishGameToRoom(GAME_STARTED.name(), roomNumber);

		log.info("\n [게임 시작] \n== redis 저장 ==\n [{}번 방]", roomNumber);

		// 1라운드 개시
		startRound(roomNumber);

	}

	public void startRound(String roomNumber) {

		// 라운드 개시 요청이 들어올 경우 기존 타이머는 초기화
		ScheduledFuture<?> oldTask = roundTasks.get(roomNumber);
		if (oldTask != null && !oldTask.isCancelled()) {
			oldTask.cancel(true);
		}

		Long roundTime = ((GameDto)gameRedisTemplate.opsForValue().get(roomNumber)).getRoundTime();

		ScheduledFuture<?> newTask = scheduler.scheduleWithFixedDelay(() -> proceedRound(roomNumber), 0, roundTime,
			TimeUnit.SECONDS);
		roundTasks.put(roomNumber, newTask);
	}

	// 모든 인원이 레디 상태인지 확인하기 위한 메서드
	private boolean checkAllReady(RoomDto roomDto) {
		return roomDto.getMembers().stream().allMatch(MemberInGameDto::getIsReady);
	}

	// 랜덤 키워드 하나 뽑아오는 메서드
	private Keyword getRandomKeyword() {
		long count = keywordRepository.count();
		long randomIndex = new Random().nextLong(count);
		return keywordRepository.findById(randomIndex).orElseThrow(() -> new BaseException(KEYWORD_NOT_FOUND));
	}

	// 라운드 개시 요청 시 실행되는 Task
	private void proceedRound(String roomNumber) {
		// 해당 게임 정보 가져오기
		GameDto gameDto = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 최대 라운드 수에 도달했는지 확인
		if (gameDto.getCurrentRound() >= gameDto.getRoundLimit()) {
			// 게임 종료
			log.info("\n [{}번 방 게임 종료] \n", roomNumber);
			return;
		}

		// db 에서 랜덤 키워드 하나 뽑아오기
		Keyword keyword = getRandomKeyword();

		// Redis 에 변경되는 정보 저장
		gameDto = gameDto.toBuilder()
			.currentRound(gameDto.getCurrentRound() + 1)
			.keyword(keyword.getWord())
			.answerCount(0L)
			.build();
		gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// pub to room
		GameRoundDto gameRoundDto = gameDto.toRoundDto();
		messagePublisher.publishGameToRoom(ROUND_STARTED.name(), roomNumber, gameRoundDto);
	}
}