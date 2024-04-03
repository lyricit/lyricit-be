package com.ssafy.lyricit.search.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;

import com.ssafy.lyricit.search.dto.AnswerSearchDto;
import com.ssafy.lyricit.search.dto.ElasticSearchResponseDto;
import com.ssafy.lyricit.search.dto.LyricSearchDto;

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

		// v2
		// LyricSearchDto lyricSearchDto = LyricSearchDto.builder()
		// 	.query(LyricSearchDto.Query.builder()
		// 		.match_phrase(LyricSearchDto.MatchPhrase.builder()
		// 			.lyrics(lyrics)
		// 			.build())
		// 		.build())
		// 	.build();

		String lyricSearchV3 = "{\"query\": {\"bool\": {\"must\": [{\"match_phrase\": {\"lyrics\": \""+ lyrics +"\" }},{\"match\": {\"lyrics.ngram\": \""+ lyrics +"\"}}]}}}";

		return getSearchResponse(lyricSearchV3);
	}

	public ElasticSearchResponseDto searchAnswer(String lyrics, String title, String artist) {

		// List<Object> must = new ArrayList<>();
		// must.add(AnswerSearchDto.Match.builder().match(AnswerSearchDto.Title.builder().title(title).build()).build());
		// must.add(
		// 	AnswerSearchDto.Match.builder().match(AnswerSearchDto.Artist.builder().artist(artist).build()).build());
		// must.add(AnswerSearchDto.MatchPhrase.builder()
		// 	.match_phrase(AnswerSearchDto.Lyrics.builder().lyrics(lyrics).build())
		// 	.build());
		//
		// AnswerSearchDto answerSearchDto = AnswerSearchDto.builder()
		// 	.query(AnswerSearchDto.Query.builder()
		// 		.bool(AnswerSearchDto.Bool.builder()
		// 			.Must(must)
		// 			.build())
		// 		.build())
		// 	.build();

		String answerSearchV3 = "{\"query\": {\"bool\": {\"must\": [{\"match\": {\"title\": \""+ title +"\"}}, {\"match\" : {\"artist\" : \""+ artist +"\"}}, {\"bool\": {\"must\": [{\"match_phrase\": {\"lyrics\": \"" + lyrics + "\"}}, {\"match\": {\"lyrics.ngram\": \"" + lyrics + "\" }}]}}]}}}";

		return getSearchResponse(answerSearchV3);
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
