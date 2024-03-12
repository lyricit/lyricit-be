package com.ssafy.lyricit.member.domain;

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
public class Member extends BaseEntity {
	@NonNull
	private String nickname;
}
