package com.ssafy.lyricit.game.dto;

import lombok.Builder;

@Builder
public record GameChatResponseDto(
	String nickname,
	String content
) {
}
