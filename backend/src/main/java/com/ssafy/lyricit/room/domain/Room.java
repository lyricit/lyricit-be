package com.ssafy.lyricit.room.domain;

import com.ssafy.lyricit.common.BaseEntity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Room extends BaseEntity {
	@NonNull
	private Long roomNumber;

	@NonNull
	private String name;

	@NonNull
	private String password;

	@NonNull
	private Long roundTime;

	@NonNull
	private Long roundCount;

	@NonNull
	private Long playerLimit;

	@NonNull
	private String leaderId;

	@NonNull
	private Boolean isPublic;

	@NonNull
	private Boolean isPlaying;
}
