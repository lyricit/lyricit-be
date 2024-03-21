package com.ssafy.lyricit.member.dto;

import lombok.Builder;

@Builder
public record MemberDto(
	String memberId,
	String nickname,
	String decoType,
	String faceType,
	String decoColor,
	String skinColor
) {
}
