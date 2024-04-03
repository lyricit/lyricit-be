package com.ssafy.lyricit.game.constant;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ScoreValue {
	SCORE_1ST(2000L),
	SCORE_2ND(1500L),
	SCORE_3RD(1000L),
	SCORE_4TH(500L),
	SCORE_5TH(300L),
	SCORE_6TH(100L);

	private final Long value;
}
