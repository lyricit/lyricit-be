package com.ssafy.lyricit.member.service;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.repository.MemberRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class MemberService {
	private final MemberRepository memberRepository;

	public void createMember(MemberDto memberDto) {
		memberRepository.save(memberDto.toEntity());
	}

	public MemberDto findMemberById(String memberId) {
		Member member = memberRepository.findById(memberId).orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
		return member.toDto();
	}

	public List<MemberDto> findAllMembers() {
		return memberRepository.findAll().stream()
			.map(Member::toDto)
			.collect(Collectors.toList());
	}
}
