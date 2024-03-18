package com.ssafy.lyricit.member.controller;

import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Controller;

import com.ssafy.lyricit.member.aspect.MemberIdCheck;
import com.ssafy.lyricit.member.service.MemberService;

import lombok.RequiredArgsConstructor;

@Controller
@RequiredArgsConstructor
public class MemberController {
	private final MemberService memberService;
	private final SimpMessagingTemplate template; //특정 Broker로 메세지를 전달

	// @MessageMapping("/enter/{roomNumber}")
	// public void login(MemberRequestDto memberRequest, SimpMessageHeaderAccessor headerAccessor) {// + join
	// 	String memberId = (String)headerAccessor.getSessionAttributes().get("memberId");
	// 	memberService.createMember(memberRequest);
	// 	template.convertAndSend("/sub/chat/room/" + messageResponse.roomNumber(), messageResponse);
	// }

	@MemberIdCheck
	@MessageMapping("/members/{memberId}")
	public void getMember(@DestinationVariable String memberId) {
		template.convertAndSend("/sub/lounge", memberService.findMemberById(memberId));
	}

	@MemberIdCheck
	@MessageMapping("/members")
	public void getAllMembers() {
		template.convertAndSend("/sub/lounge", memberService.findAllMembers());
	}
}
