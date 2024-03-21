package com.ssafy.lyricit.chat.dto;

import lombok.Builder;

@Builder
public record RoomChatRequestDto(
	String roomNumber,
	String nickname,
	String content
) {
}
