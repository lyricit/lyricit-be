package com.ssafy.lyricit.message.dto;

import lombok.Builder;

@Builder
public record MessageRequestDto(
	Long roomNumber,
	String content
) {
}
