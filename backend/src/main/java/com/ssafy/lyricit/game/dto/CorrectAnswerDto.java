package com.ssafy.lyricit.game.dto;

import com.ssafy.lyricit.member.dto.MemberDto;

import lombok.Builder;

@Builder
public record CorrectAnswerDto(
	MemberDto member,
	Long score,
	Long totalScore,
	String answerTitle,
	String answerArtist
) {
}
