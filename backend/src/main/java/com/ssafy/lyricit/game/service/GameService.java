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
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.game.dto.GameInfoDto;
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
			.room(roomDto)
			.currentRound(0L)
			.keyword("")
			.answerCount(0L)
			.build();

	    gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// pub to lounge
		RoomOutsideDto roomOutsideDto = roomDto.toOutsideDto(roomNumber);
		messagePublisher.publishRoomToLounge(ROOM_UPDATED.name(), roomOutsideDto);

		// pub to room
		GameInfoDto gameInfoDto = gameDto.toInfoDto(roomNumber);
		messagePublisher.publishGameToRoom(GAME_STARTED.name(), roomNumber, gameInfoDto);

		log.info("\n [게임 시작] \n== redis 저장 ==\n [{}번 방]", roomNumber);

	}

	// 모든 인원이 레디 상태인지 확인하기 위한 메서드
	private boolean checkAllReady(RoomDto roomDto) {
		return roomDto.getMembers().stream().allMatch(member -> member.getIsReady());
	}
}
