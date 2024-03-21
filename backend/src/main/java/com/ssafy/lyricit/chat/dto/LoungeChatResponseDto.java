package com.ssafy.lyricit.chat.dto;

import java.time.LocalDateTime;

import lombok.Builder;

@Builder
public record LoungeChatResponseDto(
	String nickname,
	String content,
	LocalDateTime time
) {
}
