package com.ssafy.lyricit.member.domain;

import com.ssafy.lyricit.common.BaseEntity;
import com.ssafy.lyricit.member.dto.MemberDto;

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
	private String nickname;
	@NonNull
	private String deco;
	@NonNull
	private String face;
	@NonNull
	private String decoColor;
	@NonNull
	private String faceColor;

	public MemberDto toDto() {
		return MemberDto.builder()
			.nickname(nickname)
			.deco(deco)
			.face(face)
			.decoColor(decoColor)
			.faceColor(faceColor)
			.build();
	}
}
