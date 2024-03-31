package com.ssafy.lyricit.game.service;

import static com.ssafy.lyricit.common.type.EventType.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import org.quartz.SchedulerException;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.game.dto.HighlightDto;
import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.service.RoomService;
import com.ssafy.lyricit.round.job.RoundService;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class GameService {
	private final RedisTemplate<String, RoomDto> roomRedisTemplate;
	private final RedisTemplate<String, GameDto> gameRedisTemplate;
	private final RoomService roomService;
	private final RoundService roundService;
	private final MessagePublisher messagePublisher;

	public void startGame(String memberId, String roomNumber) throws SchedulerException {
		RoomDto roomDto = validateGameStart(memberId, roomNumber);

		// update isPlaying
		roomDto.setIsPlaying(true);
		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);

		// save to game db
		gameRedisTemplate.opsForValue().set(roomNumber, GameDto.create(roomDto, HighlightDto.create()));

		// pub
		messagePublisher.publishRoomToLounge(ROOM_UPDATED.name(), roomDto.toOutsideDto(roomNumber));
		messagePublisher.publishGameToRoom(GAME_STARTED.name(), roomNumber);

		// round start
		roundService.addRoundSchedule(roomNumber);
	}

	private RoomDto validateGameStart(String memberId, String roomNumber) {
		RoomDto roomDto = roomService.validateRoom(roomNumber);
		checkGamePlaying(roomNumber);

		// check leader
		if (Boolean.FALSE.equals(memberId.equals(roomDto.getLeaderId()))) {
			throw new BaseException(NOT_LEADER);
		}
		// check ready
		if (!isAllReady(roomDto)) {
			throw new BaseException(NOT_ALL_READY);
		}

		return roomDto;
	}

	private void checkGamePlaying(String roomNumber) {
		if (Boolean.TRUE.equals(gameRedisTemplate.hasKey(roomNumber)) ||
			roomService.validateRoom(roomNumber).getIsPlaying()) {
			throw new BaseException(GAME_ALREADY_PLAYING);
		}
	}

	// check all members are ready except leader
	private boolean isAllReady(RoomDto roomDto) {
		return roomDto.getMembers().stream()
			.filter(member -> !member.getMember().memberId().equals(roomDto.getLeaderId()))
			.allMatch(MemberInGameDto::getIsReady);
	}
}
