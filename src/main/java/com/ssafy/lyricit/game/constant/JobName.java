package com.ssafy.lyricit.game.constant;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum JobName {
	START_JOB("startJob:"),
	END_JOB("endJob:"),
	JOB_GROUP("roundJobs"),
	ROOM_NUMBER("roomNumber");

	private final String value;
}
