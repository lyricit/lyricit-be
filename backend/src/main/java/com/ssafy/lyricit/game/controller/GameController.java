package com.ssafy.lyricit.game.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.game.service.GameService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "게임 컨트롤러", description = "게임 관련 기능들이 포함되어 있음")
@RestController
@RequestMapping("/v1/games")
@RequiredArgsConstructor
public class GameController {
	private final GameService gameService;

	@Operation(summary = "게임 시작")
	@PostMapping("/{roomNumber}")
	public ResponseEntity<Void> startGame(
		@RequestHeader String memberId,
		@PathVariable String roomNumber) {
		gameService.startGame(memberId, roomNumber);
		return ResponseEntity.ok().build();
	}


}
