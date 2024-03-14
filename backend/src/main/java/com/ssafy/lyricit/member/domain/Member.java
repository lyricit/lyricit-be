package com.ssafy.lyricit.member.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Member {
	@NonNull
	private String nickname;
	@NonNull
	private String deco;
	@NonNull
	private String face;
	@NonNull
	private String decoColor;
	@NonNull
	private String faceColor;
}
