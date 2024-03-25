package com.ssafy.lyricit.game.service;

import static com.ssafy.lyricit.common.type.EventType.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

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
import com.ssafy.lyricit.game.dto.GameRoundDto;
import com.ssafy.lyricit.game.repository.KeywordRepository;
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
		messagePublisher.publishGameToRoom(GAME_STARTED.name(), roomNumber, null);

		log.info("\n [게임 시작] \n== redis 저장 ==\n [{}번 방]", roomNumber);


		// 1라운드 시작
		startRound(roomNumber);


	}


	public void startRound(String roomNumber) {

		// 해당 게임 정보 가져오기
		GameDto gameDto = (GameDto)gameRedisTemplate.opsForValue().get(roomNumber);

		// 최대 라운드 수에 도달했는지 확인
		if (gameDto.getCurrentRound() > gameDto.getRoundLimit()) {
			// 게임 종료
			return;
		}

		// db 에서 랜덤 키워드 하나 뽑아오기
		Keyword keyword = keywordRepository.findRandomKeyword();

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

		log.info("\n [라운드 개시] \n== redis 저장 ==\n [{}번 방]", roomNumber);

	}


	// 모든 인원이 레디 상태인지 확인하기 위한 메서드
	private boolean checkAllReady(RoomDto roomDto) {
		return roomDto.getMembers().stream().allMatch(member -> member.getIsReady());
	}
}
