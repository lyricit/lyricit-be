package com.ssafy.lyricit.chat.dto;

import lombok.Builder;

@Builder
public record LoungeChatRequestDto(
	String nickname,
	String content
) {
}
