package com.ssafy.lyricit.dictionary.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.dictionary.dto.TrackDetailDto;
import com.ssafy.lyricit.dictionary.dto.TrackDto;
import com.ssafy.lyricit.dictionary.service.DictionaryService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "사전 컨트롤러", description = "키워드에 맞춘 사전 검색 기능이 포함되어 있음")
@RestController
@RequiredArgsConstructor
@RequestMapping("/v1/dictionary")
public class DictionaryController {

	private final DictionaryService dictionaryService;

	@Operation(summary = "사전 키워드 검색")
	@GetMapping("/search")
	public ResponseEntity<List<TrackDto>> searchKeyword(@RequestParam("keyword") String keyword) {
		return ResponseEntity.ok(dictionaryService.searchKeyword(keyword));
	}

	@Operation(summary = "노래 상세 조회")
	@GetMapping("/detail/{trackId}")
	public ResponseEntity<TrackDetailDto> getTrackDetail(@PathVariable("trackId") String trackId) {
		return ResponseEntity.ok(dictionaryService.readTrackDeatil(trackId));
	}
}
