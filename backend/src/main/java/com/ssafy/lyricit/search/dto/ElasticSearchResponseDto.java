package com.ssafy.lyricit.search.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ElasticSearchResponseDto {
	private int took;
	private boolean timed_out;
	private Shards _shards;
	private Hits hits;


	@Getter
	@Setter
	public static class Shards {
		private int total;
		private int successful;
		private int skipped;
		private int failed;
	}

	@Getter
	@Setter
	public static class Hits {
		private Total total;
		private double max_score;
		private Hit[] hits;
	}

	@Getter
	@Setter
	public static class Total {
		private int value;
		private String relation;
	}

	@Getter
	@Setter
	public static class Hit {
		private String _index;
		private String _id;
		private double _score;
		private Source _source;
	}

	@Getter
	@Setter
	public static class Source {
		private String code;
		private String title;
		private String artist;
		private SpotifyInfo spotify_info;
		private String lyrics;
	}

	@Getter
	@Setter
	public static class SpotifyInfo {
		private String id;
		private String title;
		private String artist;
		private String preview_play_url;
		private AlbumUrl album_url;
		private String popularity;
	}

	@Getter
	@Setter
	public static class AlbumUrl {
		private String image_size_300;
		private String image_size_64;
	}
}














