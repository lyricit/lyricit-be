package com.ssafy.lyricit.room.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.ssafy.lyricit.member.dto.MemberInGameDto;

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
@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS)
// on redis
public class RoomDto {
	private String roomId;
	private String name;
	private String password;
	private Long playerCount;
	private Long playerLimit;
	private Boolean isPlaying;
	private Boolean isPublic;
	private Long roundLimit;
	private Long roundTime;
	private List<MemberInGameDto> members;

	public RoomInsideDto toInsideDto(String roomNumber) {
		return RoomInsideDto.builder()
			.roomNumber(roomNumber)
			.name(name)
			.roundLimit(roundLimit)
			.roundTime(roundTime)
			.isPublic(isPublic)
			.members(members)
			.build();
	}

	public RoomOutsideDto toOutsideDto(String roomNumber) {
		return RoomOutsideDto.builder()
			.roomNumber(roomNumber)
			.name(name)
			.isPlaying(isPlaying)
			.isPublic(isPublic)
			.playerCount(playerCount)
			.playerLimit(playerLimit)
			.build();
	}

	@Override
	public String toString() {
		String membersNicknames = members.stream()
			.map(memberInGameDto -> memberInGameDto.member().nickname())
			.toList()
			.toString();

		return "{" +
			"\n\tname='" + name + '\'' +
			", \n\tplayerCount=" + playerCount +
			", \n\tplayerLimit=" + playerLimit +
			", \n\tisPlaying=" + isPlaying +
			", \n\tisPublic=" + isPublic +
			", \n\troundLimit=" + roundLimit +
			", \n\troundTime=" + roundTime +
			", \n\tmembers=" + membersNicknames +
			"\n}";
	}
}
