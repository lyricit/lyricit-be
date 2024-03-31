package com.ssafy.lyricit.chat.dto;

import java.time.LocalDateTime;

import lombok.Builder;

@Builder
public record RoomChatRequestDto(
	String roomNumber,
	String memberId,
	String nickname,
	String content
) {
	public RoomChatResponseDto toResponseDto() {
		return RoomChatResponseDto.builder()
			.roomNumber(roomNumber)
			.nickname(nickname)
			.content(content)
			.time(LocalDateTime.now())
			.build();
	}
}
