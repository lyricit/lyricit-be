package com.ssafy.lyricit.round.dto;

import lombok.Builder;

@Builder
public record RoundDto(
	Long currentRound,
	String keyword
) {
}
