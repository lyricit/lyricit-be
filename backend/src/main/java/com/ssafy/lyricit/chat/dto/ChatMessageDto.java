package com.ssafy.lyricit.chat.dto;

import lombok.Builder;

@Builder
public record ChatMessageDto(
	String content
) {
}
