package com.ssafy.lyricit.game.dto;

import java.util.ArrayList;
import java.util.List;

import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.round.dto.RoundDto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class GameDto {
	private Long playerCount;
	private Long roundTime;
	private Long currentRound;
	private String keyword;
	private HighlightDto highlightDto;
	private List<String> correctMembers;
	private List<ScoreDto> members;
	private Boolean isGameEnded;

	public RoundDto toRoundDto() {
		return RoundDto.builder()
			.currentRound(currentRound)
			.keyword(keyword)
			.build();
	}

	public static GameDto create(RoomDto roomDto, HighlightDto initialHighlightInfo) {
		return GameDto.builder()
			.playerCount(roomDto.getPlayerCount())
			.roundTime(roomDto.getRoundTime())
			.currentRound(0L)
			.keyword("")
			.correctMembers(new ArrayList<>())
			.highlightDto(initialHighlightInfo)
			.members(roomDto.getMembers().stream()
				.map(MemberInGameDto::toScoreDto)
				.toList())
			.isGameEnded(false)
			.build();
	}
}
