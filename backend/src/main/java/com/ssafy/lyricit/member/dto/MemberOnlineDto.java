package com.ssafy.lyricit.member.dto;

import lombok.Builder;

@Builder
public record MemberOnlineDto(
	String memberId,
	String nickname
) {
}
