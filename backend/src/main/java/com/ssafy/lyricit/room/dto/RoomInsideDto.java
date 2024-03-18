package com.ssafy.lyricit.room.dto;

import java.util.List;

import com.ssafy.lyricit.member.dto.MemberInGameDto;

import lombok.Builder;

@Builder
public record RoomInsideDto(
	String roomNumber,
	String name,
	Long roundLimit,
	Long roundTime,
	Boolean isPublic,
	List<MemberInGameDto> members
) {
}
