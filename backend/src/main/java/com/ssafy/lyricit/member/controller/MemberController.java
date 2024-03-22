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
import com.ssafy.lyricit.member.dto.MemberIdDto;
import com.ssafy.lyricit.member.dto.MemberRequestDto;
import com.ssafy.lyricit.member.service.MemberService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "로그인 컨트롤러", description = "로그인 및 회원가입 관련 기능이 포함되어 있음")
@RestController
@RequiredArgsConstructor
@RequestMapping("/v1/members")
public class MemberController {
	private final MemberService memberService;

	@Operation(summary = "로그인 or 회원가입")
	@PostMapping("/login")
	public ResponseEntity<MemberIdDto> login(@RequestBody MemberRequestDto memberRequestDto) {
		return ResponseEntity.ok(memberService.authorize(memberRequestDto));
	}

	@Operation(summary = "회원정보 조회 (테스트)")
	@GetMapping("/{memberId}")
	public ResponseEntity<MemberDto> getMember(@PathVariable String memberId) {
		return ResponseEntity.ok(memberService.findMemberById(memberId));
	}

	@Operation(summary = "회원정보 전체 조회 (테스트)")
	@GetMapping
	public ResponseEntity<List<MemberDto>> getAllMembers() {
		return ResponseEntity.ok(memberService.findAllMembers());
	}
}
