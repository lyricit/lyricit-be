package com.ssafy.lyricit.common;

import lombok.Builder;

@Builder
public record EventDto(
	String type,
	String roomNumber
) {
}
