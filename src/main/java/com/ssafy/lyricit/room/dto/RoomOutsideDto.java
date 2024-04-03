package com.ssafy.lyricit.room.dto;

import lombok.Builder;

@Builder
public record RoomOutsideDto(
	String roomNumber,
	String name,
	Boolean isPlaying,
	Boolean isPublic,
	Long playerCount,
	Long playerLimit
) {
}
