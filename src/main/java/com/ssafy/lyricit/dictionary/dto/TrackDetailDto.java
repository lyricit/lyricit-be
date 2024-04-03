package com.ssafy.lyricit.dictionary.dto;

import lombok.Builder;

@Builder
public record TrackDetailDto (
	String title,
	String artist,
	String imageUrl,
	String lyrics
){
}
