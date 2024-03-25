package com.ssafy.lyricit.game.dto;

import com.ssafy.lyricit.room.dto.RoomDto;

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
public class GameDto {
	private RoomDto room;
	private Long currentRound;
	private String keyword;
	private Long answerCount;

	public GameInfoDto toInfoDto(String roomNumber) {
		return GameInfoDto.builder()
			.roomNumber(roomNumber)
			.members(room.getMembers())
			.build();
	}
}
