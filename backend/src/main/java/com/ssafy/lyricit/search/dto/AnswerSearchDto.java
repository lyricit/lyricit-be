package com.ssafy.lyricit.search.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AnswerSearchDto {
	private Query query;

	@Getter
	@Setter
	@Builder
	public static class Query {
		private Bool bool;
	}

	@Getter
	@Setter
	@Builder
	public static class Bool {
		private List<Object> Must;
	}

	@Getter
	@Setter
	@Builder
	public static class Match {
		private Object match;
	}

	@Getter
	@Setter
	@Builder
	public static class MatchPhrase {
		private Lyrics match_phrase;
	}

	@Getter
	@Setter
	@Builder
	public static class Lyrics {
		private String lyrics;
	}

	@Getter
	@Setter
	@Builder
	public static class Title {
		private String title;
	}

	@Getter
	@Setter
	@Builder
	public static class Artist {
		private String artist;
	}

}
