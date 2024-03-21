package com.ssafy.lyricit.game.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.game.service.GameService;

import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/v1/games")
@RequiredArgsConstructor
public class GameController {
	private final GameService gameService;

	@PostMapping
	public ResponseEntity<Void> startGame(
		@RequestHeader String memberId,
		@RequestBody String roomNumber) {
		gameService.startGame(memberId, roomNumber);
		return ResponseEntity.ok().build();
	}


}
