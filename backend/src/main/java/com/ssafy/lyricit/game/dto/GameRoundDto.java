package com.ssafy.lyricit.game.dto;

import lombok.Builder;

@Builder
public record GameRoundDto(
	Long currentRound,
	String keyword
) {
}
