package com.ssafy.lyricit.game.dto;

import java.util.List;

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
	private Long roundLimit;
	private Long currentRound;
	private String keyword;
	private HighlightDto highlightInfo;
	private List<String> correctMembers;
	private List<ScoreDto> members;

	public GameRoundDto toRoundDto() {
		return GameRoundDto.builder()
			.currentRound(currentRound)
			.keyword(keyword)
			.build();
	}
}
