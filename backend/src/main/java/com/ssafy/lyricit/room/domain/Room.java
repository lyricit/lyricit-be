package com.ssafy.lyricit.room.domain;

import java.util.Collections;

import com.ssafy.lyricit.common.BaseEntity;
import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.room.dto.RoomDto;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
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
public class Room extends BaseEntity {
	@NonNull
	private String name;

	@NonNull
	private String password;

	@NonNull
	private Long playerLimit;

	@NonNull
	private Long roundLimit;

	@NonNull
	private Long roundTime;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "member_id")
	private Member member;

	public RoomDto toDto(MemberInGameDto memberInGameDto) {
		return RoomDto.builder()
			.roomId(getId())
			.name(name)
			.password(password)
			.playerCount(1L)
			.playerLimit(playerLimit)
			.isPlaying(false)
			.isPublic(password.isBlank())
			.roundLimit(roundLimit)
			.roundTime(roundTime)
			.members(Collections.singletonList(memberInGameDto))
			.build();
	}
}
