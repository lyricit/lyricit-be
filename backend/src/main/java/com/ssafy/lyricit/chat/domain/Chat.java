package com.ssafy.lyricit.chat.domain;

import com.ssafy.lyricit.common.BaseEntity;
import com.ssafy.lyricit.chat.dto.ChatResponseDto;

import jakarta.persistence.Entity;
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
@Entity
public class Chat extends BaseEntity {
	private String roomNumber;
	private String nickname;
	private String content;

	public ChatResponseDto toResponseDto() {
		return ChatResponseDto.builder()
			.roomNumber(roomNumber)
			.nickname(nickname)
			.content(content)
			.time(getCreatedAt())
			.build();
	}
}
