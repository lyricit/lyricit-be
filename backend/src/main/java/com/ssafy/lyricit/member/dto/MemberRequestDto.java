package com.ssafy.lyricit.member.dto;

import com.ssafy.lyricit.member.domain.Member;

import lombok.Builder;

@Builder
public record MemberRequestDto(
	String nickname,
	String decoType,
	String faceType,
	String decoColor,
	String skinColor
) {
	public Member toEntity() {
		return Member.builder()
			.nickname(nickname())
			.decoType(decoType())
			.faceType(faceType())
			.decoColor(decoColor())
			.skinColor(skinColor())
			.build();
	}
}
