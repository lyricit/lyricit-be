package com.ssafy.lyricit.common.type;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum HeaderConstant {
	MEMBER_ID("memberId"),
	NICKNAME("nickname"),
	DECO_TYPE("decoType"),
	FACE_TYPE("faceType"),
	DECO_COLOR("decoColor"),
	SKIN_COLOR("skinColor");

	private final String value;
}
