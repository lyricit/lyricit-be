package com.ssafy.lyricit.member.dto;

import com.ssafy.lyricit.member.domain.Member;

import lombok.Builder;

@Builder
public record MemberDto(
	String nickname,
	String deco,
	String face,
	String decoColor,
	String faceColor
) {
	public Member toEntity() {
		return Member.builder()
			.nickname(nickname())
			.deco(deco())
			.face(face())
			.decoColor(decoColor())
			.faceColor(faceColor())
			.build();
	}
}
