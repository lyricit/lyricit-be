package com.ssafy.lyricit.common.documents;

import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import jakarta.persistence.Id;
import lombok.Getter;

@Getter
@Document(indexName = "tracks_v2")
public class TrackDocument {

	@Id
	private String id; // ElasticSearch의 _id 필드에 해당

	private String code;

	@Field(type = FieldType.Text)
	private String title;

	@Field(type = FieldType.Text)
	private String artist;

	@Field(type = FieldType.Object, name = "spotify_info")
	private SpotifyInfo spotifyInfo;

	@Field(type = FieldType.Text)
	private String lyrics;
}
