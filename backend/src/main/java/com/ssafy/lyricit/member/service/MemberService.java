package com.ssafy.lyricit.member.service;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.List;

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

	public String join(MemberDto memberDto) {
		Member member = memberDto.toEntity();
		System.out.println("가입한 유저 \n" + member);// log
		memberRepository.save(member);
		return member.getId();// 랜덤으로 새로 생성된 id 반환
	}

	public String login(String memberId, MemberDto memberDto) {
		if (!memberRepository.existsById(memberId)) {// 사용자의 id가 유효하지 않을 경우 강제 가입
			return join(memberDto);
		}

		Member member = memberRepository.findById(memberId).orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
		member.update(memberDto);// overwrite status
		System.out.println("로그인 유저 \n" + member);// log
		return memberId;
	}

	public MemberDto findMemberById(String memberId) {
		Member member = memberRepository.findById(memberId).orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
		System.out.println("찾은 유저 \n" + member);// log
		return member.toDto();
	}

	public List<MemberDto> findAllMembers() {
		return memberRepository.findAll().stream()
			.map(Member::toDto)
			.toList();
	}
}
