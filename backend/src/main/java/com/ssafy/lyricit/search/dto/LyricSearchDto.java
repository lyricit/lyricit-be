package com.ssafy.lyricit.search.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class LyricSearchDto {
	private Query query;

	@Getter
	@Setter
	@Builder
	public static class Query {
		private MatchPhrase match_phrase;
	}

	@Getter
	@Setter
	@Builder
	public static class MatchPhrase {
		private String lyrics;
	}

}
