package com.ssafy.lyricit.member.dto;

import lombok.Builder;

@Builder
public record MemberInGameDto(
	MemberDto memberDto,
	Boolean isReady,
	Long score
) {
}
