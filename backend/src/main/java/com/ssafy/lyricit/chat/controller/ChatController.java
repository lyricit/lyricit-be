package com.ssafy.lyricit.chat.controller;

import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.stereotype.Controller;

import com.ssafy.lyricit.chat.dto.ChatRequestDto;
import com.ssafy.lyricit.chat.service.ChatService;
import com.ssafy.lyricit.member.aspect.MemberIdCheck;

import lombok.RequiredArgsConstructor;

@Controller
@RequiredArgsConstructor
public class ChatController {
	private final ChatService chatService;

	// Client가 SEND할 수 있는 경로
	// stompConfig에서 설정한 applicationDestinationPrefixes와 @MessageMapping 경로가 병합됨
	// "/pub/chat/enter"
	@MemberIdCheck
	@MessageMapping("/chat/enter")
	public void enter(ChatRequestDto chatRequest) {
		chatService.sendEnterMessage(chatRequest);
	}

	@MemberIdCheck
	@MessageMapping("/chat/exit")
	public void exit(ChatRequestDto chatRequest) {
		chatService.sendExitMessage(chatRequest);
	}

	@MemberIdCheck
	@MessageMapping("/chat/message")
	public void chat(ChatRequestDto chatRequest) {
		chatService.sendChatMessage(chatRequest);
	}
}
