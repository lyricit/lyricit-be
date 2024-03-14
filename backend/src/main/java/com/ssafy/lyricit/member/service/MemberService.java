package com.ssafy.lyricit.member.service;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.repository.MemberRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
// @Transactional
public class MemberService {
	private final MemberRepository memberRepository;

	public Member addMember(MemberDto memberDto) {
		Member member = Member.builder()
			.nickname(memberDto.nickname())
			.deco("deco")
			.face("face")
			.decoColor("decoColor")
			.faceColor("faceColor")
			.build();
		memberRepository.addMember(member);
		return member;
	}

	public Map<String, Member> findAllMembers() {
		return memberRepository.findAllMembers();
	}
}
