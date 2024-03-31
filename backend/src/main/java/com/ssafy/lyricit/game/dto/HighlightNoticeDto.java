package com.ssafy.lyricit.game.dto;

import lombok.Builder;

@Builder
public record HighlightNoticeDto(
	String memberId,
	String lyric,
	Long timeLimit
) {
}
