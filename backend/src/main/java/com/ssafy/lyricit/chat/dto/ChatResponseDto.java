package com.ssafy.lyricit.chat.dto;

import java.time.LocalDateTime;

import lombok.Builder;

@Builder
public record ChatResponseDto(
	String roomNumber,
	String nickname,
	String content,
	LocalDateTime time
) {
}
