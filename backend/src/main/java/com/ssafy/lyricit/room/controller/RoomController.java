package com.ssafy.lyricit.room.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.room.dto.RoomOutsideDto;
import com.ssafy.lyricit.room.dto.RoomRequestDto;
import com.ssafy.lyricit.room.service.RoomService;

import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/rooms")
@RequiredArgsConstructor
public class RoomController {
	private final RoomService roomService;

	@PostMapping
	public ResponseEntity<String> addRoom(
		@RequestHeader String memberId,
		@RequestBody RoomRequestDto roomRequest) {
		return ResponseEntity.ok(roomService.createRoom(memberId, roomRequest));
	}

	// 방 조회
	@GetMapping("/{roomNumber}")
	public ResponseEntity<RoomOutsideDto> getRoom(
		@RequestHeader String memberId,
		@PathVariable String roomNumber) {
		return ResponseEntity.ok(roomService.readRoomByRoomNumber(roomNumber));
	}

	// 방 목록 조회
	@GetMapping
	public ResponseEntity<List<RoomOutsideDto>> getAllRooms(@RequestHeader String memberId) {
		return ResponseEntity.ok(roomService.readAllRooms());
	}
}
