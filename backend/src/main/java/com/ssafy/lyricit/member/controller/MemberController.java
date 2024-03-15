package com.ssafy.lyricit.member.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.service.MemberService;

import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/members")
@RequiredArgsConstructor
public class MemberController {
	private final MemberService memberService;

	@PostMapping("/join")
	public ResponseEntity<Void> join(@RequestBody MemberDto memberDto) {
		memberService.createMember(memberDto);
		return ResponseEntity.ok().build();
	}

	@GetMapping("/{memberId}")
	public ResponseEntity<MemberDto> getMember(@PathVariable String memberId) {
		return ResponseEntity.ok(memberService.findMemberById(memberId));
	}

	@GetMapping
	public ResponseEntity<List<MemberDto>> getAllMembers() {
		return ResponseEntity.ok(memberService.findAllMembers());
	}
}
