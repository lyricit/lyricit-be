package com.ssafy.lyricit.member.domain;

import java.util.ArrayList;
import java.util.List;

import com.ssafy.lyricit.common.BaseEntity;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.member.dto.MemberRequestDto;
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
	private String decoType;

	@NonNull
	private String faceType;

	@NonNull
	private String decoColor;

	@NonNull
	private String skinColor;

	@OneToMany(mappedBy = "member", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
	@Builder.Default
	private List<Room> rooms = new ArrayList<>();

	public void update(MemberRequestDto memberRequestDto) {
		this.nickname = memberRequestDto.nickname();
		this.decoType = memberRequestDto.decoType();
		this.faceType = memberRequestDto.faceType();
		this.decoColor = memberRequestDto.decoColor();
		this.skinColor = memberRequestDto.skinColor();
	}

	public MemberDto toDto() {
		return MemberDto.builder()
			.memberId(getId())
			.nickname(nickname)
			.decoColor(decoType)
			.faceType(faceType)
			.decoColor(decoColor)
			.skinColor(skinColor)
			.build();
	}

	public MemberInGameDto toInGameDto() {
		return MemberInGameDto.builder()
			.member(toDto())
			.isReady(false)
			.score(0L)
			.build();
	}
}
