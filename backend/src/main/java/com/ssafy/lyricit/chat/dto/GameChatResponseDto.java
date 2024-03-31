package com.ssafy.lyricit.chat.dto;

import lombok.Builder;

@Builder
public record GameChatResponseDto(
	String nickname,
	String content
) {
}
