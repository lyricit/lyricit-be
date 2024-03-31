package com.ssafy.lyricit.room.controller;

import java.util.List;

import org.quartz.SchedulerException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.game.service.GameService;
import com.ssafy.lyricit.room.dto.RoomInsideDto;
import com.ssafy.lyricit.room.dto.RoomOutsideDto;
import com.ssafy.lyricit.room.dto.RoomPasswordDto;
import com.ssafy.lyricit.room.dto.RoomRequestDto;
import com.ssafy.lyricit.room.service.RoomService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "방 컨트롤러", description = "방 CRUD 관련 기능들이 포함되어 있음")
@RestController
@RequestMapping("/v1/rooms")
@RequiredArgsConstructor
public class RoomController {
	private final RoomService roomService;
	private final GameService gameService;

	@Operation(summary = "방 생성")
	@PostMapping
	public ResponseEntity<String> addRoom(
		@RequestHeader String memberId,
		@RequestBody RoomRequestDto roomRequest) {
		return ResponseEntity.ok(roomService.createRoom(memberId, roomRequest));
	}

	@Operation(summary = "방 입장")
	@PostMapping("/{roomNumber}")
	public ResponseEntity<RoomInsideDto> enterRoom(
		@RequestHeader String memberId,
		@PathVariable String roomNumber,
		@RequestBody(required = false) RoomPasswordDto roomPasswordDto) {
		return ResponseEntity.ok(roomService.enterRoom(memberId, roomNumber, roomPasswordDto));
	}

	@Operation(summary = "방 퇴장 (방의 인원이 0이 되면 방이 삭제됨)")
	@DeleteMapping("/{roomNumber}")
	public ResponseEntity<Void> exitRoom(
		@RequestHeader String memberId,
		@PathVariable String roomNumber) {
		roomService.exitRoom(memberId, roomNumber);
		return ResponseEntity.ok().build();
	}

	@Operation(summary = "방 정보 조회 (테스트)")
	@GetMapping("/{roomNumber}")
	public ResponseEntity<RoomOutsideDto> getRoom(@PathVariable String roomNumber) {
		return ResponseEntity.ok(roomService.readRoomByRoomNumber(roomNumber));
	}

	@Operation(summary = "방 전체 정보 조회")
	@GetMapping
	public ResponseEntity<List<RoomOutsideDto>> getAllRooms() {
		return ResponseEntity.ok(roomService.readAllRooms());
	}

	@Operation(summary = "게임 준비 (on, off)")
	@PutMapping("/{roomNumber}/ready")
	public ResponseEntity<Void> ready(
		@RequestHeader String memberId,
		@PathVariable String roomNumber) {
		roomService.ready(memberId, roomNumber);
		return ResponseEntity.ok().build();
	}

	@Operation(summary = "게임 시작")
	@PostMapping("/{roomNumber}/start")
	public ResponseEntity<Void> startGame(
		@RequestHeader String memberId,
		@PathVariable String roomNumber) throws SchedulerException {
		gameService.startGame(memberId, roomNumber);
		return ResponseEntity.ok().build();
	}
}
