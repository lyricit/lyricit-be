package com.ssafy.lyricit.room.dto;

import lombok.Builder;

@Builder
public record RoomPasswordDto(
	String password
) {
}
