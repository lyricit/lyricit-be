package com.ssafy.lyricit.room.dto;

import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.room.domain.Room;

public record RoomRequestDto(
	String name,
	String password,
	Long playerLimit,
	Long roundLimit,
	Long roundTime
) {

	public Room toEntity(Member member) {
		return Room.builder()
			.name(name)
			.password(password)
			.playerLimit(playerLimit)
			.roundLimit(roundLimit)
			.roundTime(roundTime)
			.member(member)
			.build();
	}
}
