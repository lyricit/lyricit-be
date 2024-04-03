package com.ssafy.lyricit.dictionary.dto;

import lombok.Builder;

@Builder
public record TrackDto (
	String id,
	String title,
	String artist,
	String imageUrl
){
}
