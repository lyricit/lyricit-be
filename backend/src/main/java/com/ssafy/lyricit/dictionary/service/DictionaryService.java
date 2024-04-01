package com.ssafy.lyricit.dictionary.service;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;

import com.ssafy.lyricit.common.documents.TrackDocument;
import com.ssafy.lyricit.dictionary.dto.TrackDetailDto;
import com.ssafy.lyricit.dictionary.dto.TrackDto;
import com.ssafy.lyricit.dictionary.repository.DictionaryRepository;
import com.ssafy.lyricit.exception.BaseException;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class DictionaryService {

	private final DictionaryRepository dictionaryRepository;

	public List<TrackDto> searchKeyword(String keyword) {

		List<TrackDocument> searchResults = dictionaryRepository.findByLyrics(keyword);

		return searchResults.stream()
			.map(trackDoc ->
				TrackDto.builder()
					.id(trackDoc.getId())
					.title(trackDoc.getTitle())
					.artist(trackDoc.getArtist())
					.imageUrl(trackDoc.getSpotifyInfo().getAlbumUrl().getImageSize300())
					.build())
			.collect(Collectors.toList());
	}


	public TrackDetailDto readTrackDetail(String trackId) {
		TrackDocument trackDocument = dictionaryRepository.findById(trackId).orElseThrow(() -> new BaseException(TRACK_NOT_FOUND));
		return TrackDetailDto.builder()
			.title(trackDocument.getTitle())
			.artist(trackDocument.getArtist())
			.imageUrl(trackDocument.getSpotifyInfo().getAlbumUrl().getImageSize300())
			.lyrics(trackDocument.getLyrics())
			.build();
	}
}
