package com.ssafy.lyricit.member.dto;

import lombok.Builder;

@Builder
public record MemberDto(
	String memberId,
	String nickname,
	String deco,
	String face,
	String decoColor,
	String faceColor
) {
}
