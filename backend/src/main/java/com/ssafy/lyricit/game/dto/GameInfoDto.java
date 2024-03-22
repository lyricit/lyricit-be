package com.ssafy.lyricit.game.dto;

import java.util.List;

import com.ssafy.lyricit.member.dto.MemberInGameDto;

import lombok.Builder;

@Builder
public record GameInfoDto(
	String roomNumber,
	Long playerCount,
	Long playerLimit,
	Long roundLimit,
	Long roundTime,
	String leaderId,
	Long currentRound,
	String keyword,
	Long answerCount,
	List<MemberInGameDto> members
) {}
