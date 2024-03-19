package com.ssafy.lyricit.member.service;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.dto.MemberRequestDto;
import com.ssafy.lyricit.member.repository.MemberRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class MemberService {
	private final MemberRepository memberRepository;
	private final Logger log = LoggerFactory.getLogger(this.getClass());

	public String join(MemberRequestDto memberRequestDto) {
		Member member = memberRequestDto.toEntity();
		log.info("\n [가입 완료] \n {}", member);
		memberRepository.save(member);
		return member.getId();// 랜덤으로 새로 생성된 memberId 반환
	}

	public String login(String memberId, MemberRequestDto memberRequestDto) {
		if (!memberRepository.existsById(memberId)) {// 사용자의 id가 유효하지 않을 경우 강제 가입
			return join(memberRequestDto);
		}

		Member member = memberRepository.findById(memberId).orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
		member.update(memberRequestDto);// overwrite status
		log.info("\n [로그인 완료] \n {}", member);
		return memberId;
	}

	public MemberDto findMemberById(String memberId) {
		Member member = memberRepository.findById(memberId).orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
		log.info("\n [회원 검색 결과] \n {}", member);
		return member.toDto();
	}

	public List<MemberDto> findAllMembers() {
		List<MemberDto> members = memberRepository.findAll().stream()
			.map(Member::toDto)
			.toList();
		log.info("\n [회원 목록] \n {}", members);
		return members;
	}
}
