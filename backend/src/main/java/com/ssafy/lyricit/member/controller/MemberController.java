package com.ssafy.lyricit.member.controller;

import java.util.List;
import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.service.MemberService;

import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/members")
@RequiredArgsConstructor
public class MemberController {
	private final MemberService memberService;

	@PostMapping("/join")
	public ResponseEntity<Member> join(@RequestBody MemberDto memberDto) {
		System.out.println(memberDto);
		return ResponseEntity.ok(memberService.addMember(memberDto));
	}

	@GetMapping
	public ResponseEntity<Map<String, Member>> findAllMembers() {
		return ResponseEntity.ok(memberService.findAllMembers());
	}
}
