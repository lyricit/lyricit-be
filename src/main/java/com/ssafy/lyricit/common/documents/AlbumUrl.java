package com.ssafy.lyricit.common.documents;

import org.springframework.data.elasticsearch.annotations.Field;

import lombok.Getter;

@Getter
public class AlbumUrl {

	@Field(name = "image_size_300")
	private String imageSize300;
	@Field(name = "image_size_64")
	private String imageSize64;
}
