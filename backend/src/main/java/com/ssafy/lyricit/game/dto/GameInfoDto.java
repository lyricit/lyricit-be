package com.ssafy.lyricit.game.dto;

import java.util.List;

import com.ssafy.lyricit.member.dto.MemberInGameDto;

import lombok.Builder;

@Builder
public record GameInfoDto(
	List<ScoreDto> members
) {}
