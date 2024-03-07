package com.ssafy.lyricit.member.domain;

import com.ssafy.lyricit.common.BaseEntity;

import jakarta.persistence.Entity;
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
@Entity
public class Member extends BaseEntity {
	@NonNull
	private String uuid;// uuid

	@NonNull
	private String nickname;
}
