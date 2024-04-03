package com.ssafy.lyricit.chat.domain;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import com.ssafy.lyricit.chat.dto.LoungeChatResponseDto;
import com.ssafy.lyricit.chat.dto.RoomChatResponseDto;
import com.ssafy.lyricit.common.BaseEntity;

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

	public LoungeChatResponseDto toLoungeChatResponseDto() {
		return LoungeChatResponseDto.builder()
			.nickname(nickname)
			.content(content)
			.time(LocalDateTime.now().truncatedTo(ChronoUnit.SECONDS))// todo: change to created time after save to db
			.build();
	}

	public RoomChatResponseDto toRoomChatResponseDto() {
		return RoomChatResponseDto.builder()
			.roomNumber(roomNumber)
			.nickname(nickname)
			.content(content)
			.time(LocalDateTime.now().truncatedTo(ChronoUnit.SECONDS))// todo: change to created time after save to db
			.build();
	}
}
