package com.ssafy.lyricit.member.domain;

import java.util.ArrayList;
import java.util.List;

import com.ssafy.lyricit.common.BaseEntity;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.room.domain.Room;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@ToString
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

	@OneToMany(mappedBy = "member", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
	@Builder.Default
	private List<Room> rooms = new ArrayList<>();

	public void update(MemberDto memberDto) {
		this.nickname = memberDto.nickname();
		this.deco = memberDto.deco();
		this.face = memberDto.face();
		this.decoColor = memberDto.decoColor();
		this.faceColor = memberDto.faceColor();
	}

	public MemberDto toDto() {
		return MemberDto.builder()
			.nickname(nickname)
			.deco(deco)
			.face(face)
			.decoColor(decoColor)
			.faceColor(faceColor)
			.build();
	}

	public MemberInGameDto toInGameDto() {
		return MemberInGameDto.builder()
			.memberDto(toDto())
			.isReady(false)
			.score(0L)
			.build();
	}
}
