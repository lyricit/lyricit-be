package com.ssafy.lyricit.member.service;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.dto.MemberIdDto;
import com.ssafy.lyricit.member.dto.MemberOnlineDto;
import com.ssafy.lyricit.member.dto.MemberRequestDto;
import com.ssafy.lyricit.member.repository.MemberRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class MemberService {
	private final MemberRepository memberRepository;
	private final RedisTemplate<String, String> memberRedisTemplate;
	private final Logger log = LoggerFactory.getLogger(this.getClass());

	public MemberIdDto authorize(MemberRequestDto memberRequestDto) {
		if (memberRepository.existsById(memberRequestDto.memberId())) {// login
			Member member = memberRepository.findById(memberRequestDto.memberId())
				.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
			member.update(memberRequestDto);// update status
			log.info("\n [로그인 완료] \n {}", member);
			return MemberIdDto.builder().memberId(member.getId()).build();
		}

		Member member = memberRequestDto.toEntity();// create
		memberRepository.save(member);// join
		log.info("\n [가입 완료] \n {}", member);

		return MemberIdDto.builder().memberId(member.getId()).build();
	}

	public List<MemberOnlineDto> findAllOnlineMembers() {
		Set<String> keys = memberRedisTemplate.keys("*");
		return keys.stream()
			.map(key -> MemberOnlineDto.builder()
				.memberId(key)
				.nickname(memberRedisTemplate.opsForValue().get(key))
				.build())
			.collect(Collectors.toList());
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
