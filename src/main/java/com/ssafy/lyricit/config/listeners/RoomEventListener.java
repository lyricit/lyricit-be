package com.ssafy.lyricit.config.listeners;

import org.quartz.SchedulerException;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.config.events.RoomEvent;
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.room.service.RoomService;
import com.ssafy.lyricit.round.service.RoundService;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class RoomEventListener {
	private final RoomService roomService;
	private final RoundService roundService;
	private final RedisTemplate<String, GameDto> gameRedisTemplate;

	@EventListener
	public void handleRoomEvent(RoomEvent event) throws SchedulerException {
		String roomNumber = event.getRoomNumber();
		if (!roomNumber.equals("0")) {
			if (roomService.validateRoom(roomNumber).getIsPlaying()) {
				GameDto gameDto = roundService.validateGame(roomNumber);
				gameDto.getMembers().removeIf(scoreDto -> scoreDto.getMemberId().equals(event.getMemberId()));
				if (gameDto.getMembers().isEmpty()) {
					roundService.cancelScheduledJobs(roomNumber, true);
					gameRedisTemplate.delete(roomNumber);
					roomService.exitRoom(event.getMemberId(), roomNumber);
					return;
				}
				gameRedisTemplate.opsForValue().set(roomNumber, gameDto);
				roomService.exitRoom(event.getMemberId(), roomNumber);
				return;
			}

			roomService.exitRoom(event.getMemberId(), roomNumber);
		}
	}
}
