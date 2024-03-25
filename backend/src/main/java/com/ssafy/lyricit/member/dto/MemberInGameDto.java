package com.ssafy.lyricit.member.dto;

import com.ssafy.lyricit.game.dto.ScoreDto;

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
public class MemberInGameDto {
	private MemberDto member;
	private Boolean isReady;

	public ScoreDto toScoreDto() {
		return ScoreDto.builder()
			.memberId(member.memberId())
			.score(0L)
			.build();
	}
}
