package com.ssafy.lyricit.config.listeners;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.config.events.RoomEvent;
import com.ssafy.lyricit.room.service.RoomService;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class RoomEventListener {
	private final RoomService roomService;

	@EventListener
	public void handleRoomEvent(RoomEvent event) {
		roomService.exitRoom(event.getMemberId(), event.getRoomNumber());
	}
}
