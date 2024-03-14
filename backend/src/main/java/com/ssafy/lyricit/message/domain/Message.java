package com.ssafy.lyricit.message.domain;

import java.time.LocalDateTime;

import com.ssafy.lyricit.message.dto.MessageResponseDto;

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
public class Message {
	private Long roomNumber;
	private String nickname;
	private String content;
	private LocalDateTime time;

	public MessageResponseDto toResponseDto() {
		return MessageResponseDto.builder()
			.roomNumber(roomNumber)
			.nickname(nickname)
			.content(content)
			.time(time)
			.build();
	}
}
