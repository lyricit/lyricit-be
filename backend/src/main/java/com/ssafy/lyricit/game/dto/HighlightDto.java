package com.ssafy.lyricit.game.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class HighlightDto {
	private String memberId;
	private String lyric;
	private String title;

	public HighlightNoticeDto toHighlightNoticeDto () {
		return HighlightNoticeDto.builder()
			.memberId(memberId)
			.lyric(lyric)
			.build();
	}
}
