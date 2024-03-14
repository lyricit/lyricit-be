package com.ssafy.lyricit.message.dto;

import java.time.LocalDateTime;

import lombok.Builder;

@Builder
public record MessageResponseDto(
	Long roomNumber,
	String nickname,
	String content,
	LocalDateTime time
) {
}
