package com.ssafy.lyricit.chat.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Controller;

import com.ssafy.lyricit.member.aspect.MemberIdCheck;
import com.ssafy.lyricit.chat.dto.ChatRequestDto;
import com.ssafy.lyricit.chat.dto.ChatResponseDto;
import com.ssafy.lyricit.chat.service.ChatService;

import lombok.RequiredArgsConstructor;

@Controller
@RequiredArgsConstructor
public class ChatController {
	private final ChatService chatService;
	private final SimpMessagingTemplate template; //특정 Broker로 메세지를 전달
	private final Logger log = LoggerFactory.getLogger(getClass());

	// Client가 SEND할 수 있는 경로
	// stompConfig에서 설정한 applicationDestinationPrefixes와 @MessageMapping 경로가 병합됨
	// "/pub/chat/enter"
	@MemberIdCheck
	@MessageMapping("/chat/enter")
	public void enter(ChatRequestDto chatRequest) {
		ChatResponseDto chatResponse = chatService.createEnterMessage(chatRequest);
		log.info("\n{} 입장", chatResponse.nickname());
		template.convertAndSend("/sub/rooms/" + chatResponse.roomNumber(), chatResponse);
	}

	@MemberIdCheck
	@MessageMapping("/chat/exit")
	public void exit(ChatRequestDto chatRequest) {
		ChatResponseDto chatResponse = chatService.createExitMessage(chatRequest);
		log.info("\n{} 퇴장", chatResponse.nickname());
		template.convertAndSend("/sub/rooms/" + chatResponse.roomNumber(), chatResponse);
	}

	@MemberIdCheck
	@MessageMapping("/chat/message")
	public void chat(ChatRequestDto chatRequest) {
		ChatResponseDto chatResponse = chatService.createChatMessage(chatRequest);
		log.info("\n채팅 : {}번방 \n {} : {}   ({})",
			chatResponse.roomNumber(), chatResponse.nickname(),
			chatResponse.content(), chatResponse.time());
		template.convertAndSend("/sub/rooms/" + chatResponse.roomNumber(), chatResponse);
	}
}
