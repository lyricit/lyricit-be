package com.ssafy.lyricit.Search.service;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;

import com.ssafy.lyricit.Search.dto.ElasticSearchResponseDto;
import com.ssafy.lyricit.Search.dto.LyricSearchDto;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class SearchService {
	private final WebClient webClient = WebClient.builder().build();

	@Value("${ELASTICSEARCH_URL}")
	private String url;
	@Value("${ELASTICSEARCH_API_KEY}")
	private String apiKey;

	public ElasticSearchResponseDto searchLyrics(String lyrics) {

		LyricSearchDto lyricSearchDto = LyricSearchDto.builder()
			.query(LyricSearchDto.Query.builder()
				.match_phrase(LyricSearchDto.MatchPhrase.builder()
					.lyrics(lyrics)
					.build())
				.build())
			.build();

		return getSearchResponse(lyricSearchDto);
	}

	private ElasticSearchResponseDto getSearchResponse(Object requestBody) {

		ElasticSearchResponseDto elasticSearchResponseDto = webClient.post()
			.uri(url)
			.header("Authorization", "ApiKey " + apiKey)
			.header("Content-Type", "application/json")
			.bodyValue(requestBody)
			.retrieve()
			.bodyToMono(ElasticSearchResponseDto.class)
			.block();

		return elasticSearchResponseDto;
	}
}
