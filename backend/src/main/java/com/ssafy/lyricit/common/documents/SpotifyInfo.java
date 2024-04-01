package com.ssafy.lyricit.common.documents;

import org.springframework.data.elasticsearch.annotations.Field;

import lombok.Getter;

@Getter
public class SpotifyInfo {

	private String id;
	private String title;
	private String artist;
	@Field(name = "preview_play_url")
	private String previewPlayUrl;
	@Field(name = "album_url")
	private AlbumUrl albumUrl;
	private String popularity;
}
