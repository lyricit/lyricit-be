package com.ssafy.lyricit.chat.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GameChatRequestDto {
	private String roomNumber;
	private String memberId;
	private String nickname;
	private String content;

	public GameChatResponseDto toGameChatResponseDto() {
		return GameChatResponseDto.builder()
			.nickname(nickname)
			.content(content)
			.build();
	}
}
