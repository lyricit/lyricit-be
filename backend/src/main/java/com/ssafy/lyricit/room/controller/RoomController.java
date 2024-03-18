package com.ssafy.lyricit.room.controller;

import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessageHeaderAccessor;
import org.springframework.stereotype.Controller;

import com.ssafy.lyricit.room.dto.RoomRequestDto;
import com.ssafy.lyricit.room.service.RoomService;

import lombok.RequiredArgsConstructor;

@Controller
@RequiredArgsConstructor
public class RoomController {
	private final RoomService roomService;

	// 방 개설
	@MessageMapping("/rooms/create")
	public void addRoom(SimpMessageHeaderAccessor headerAccessor, RoomRequestDto roomRequest) {
		String memberId = (String)headerAccessor.getSessionAttributes().get("memberId");
		roomService.createRoom(memberId, roomRequest);
	}

	// 방 조회
	@MessageMapping("/rooms/{roomNumber}")
	public void getRoom(@DestinationVariable String roomNumber) {
		roomService.readRoomByRoomNumber(roomNumber);
	}

	// 방 목록 조회
	@MessageMapping
	public void getAllRooms() {
		roomService.readAllRooms();
	}
}
