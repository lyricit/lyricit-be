package com.ssafy.lyricit.chat.dto;

import lombok.Builder;

@Builder
public record ChatRequestDto(
	String roomNumber,
	String nickname,
	String content
) {
}
