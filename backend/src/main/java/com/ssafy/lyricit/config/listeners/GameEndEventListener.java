package com.ssafy.lyricit.config.listeners;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.config.events.GameEndEvent;
import com.ssafy.lyricit.room.service.RoomService;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class GameEndEventListener {
	private final RoomService roomService;

	@EventListener
	public void handleGameEndEvent(GameEndEvent event) {
		String roomNumber = event.getRoomNumber();
		roomService.validateRoom(roomNumber).setIsPlaying(false);
	}
}
