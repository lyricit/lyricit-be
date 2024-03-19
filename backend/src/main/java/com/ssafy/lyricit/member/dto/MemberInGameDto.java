package com.ssafy.lyricit.member.dto;

import lombok.Builder;

@Builder
public record MemberInGameDto(
	MemberDto member,
	Boolean isReady,
	Long score
) {
}
