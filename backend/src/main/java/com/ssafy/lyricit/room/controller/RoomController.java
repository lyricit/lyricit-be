package com.ssafy.lyricit.room.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.room.domain.Room;
import com.ssafy.lyricit.room.service.RoomService;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@RestController
@RequestMapping(value = "/chat")
@RequiredArgsConstructor
@Log4j2
public class RoomController {

	private final RoomService roomService;

	// 방 목록 조회
	@GetMapping(value = "/rooms")
	public ResponseEntity<List<Room>> rooms() {
		log.info("# All Chat Rooms");
		return ResponseEntity.ok(roomService.findAllRooms());
	}

	// 방 개설
	@PostMapping(value = "/room")
	public ResponseEntity<Room> create(@RequestParam(name = "name") String name) {
		log.info("# Create Chat Room , name: " + name);
		return ResponseEntity.ok(roomService.createRoom(name));
	}

	// 방 조회
	@GetMapping("/room")
	public ResponseEntity<Room> getRoom(Long roomNumber) {
		log.info("# get Chat Room, roomNumber : " + roomNumber);
		return ResponseEntity.ok(roomService.findRoomById(roomNumber));
	}
}
