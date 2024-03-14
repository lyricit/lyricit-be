package com.ssafy.lyricit.member.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.stereotype.Repository;

import com.ssafy.lyricit.member.domain.Member;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;

@Repository
@RequiredArgsConstructor
public class MemberRepository {
	private Map<String, Member> members; // uuid : nickname

	@PostConstruct
	private void init() {
		members = new ConcurrentHashMap<>();
	}

	public  Map<String, Member> findAllMembers() {
		return members;
	}

	public void addMember(Member member) {
		members.put(UUID.randomUUID().toString(), member);
	}

	public Member findNicknameById(String memberId) {
		return members.get(memberId);
	}
}
