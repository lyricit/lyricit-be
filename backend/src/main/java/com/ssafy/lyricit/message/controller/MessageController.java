package com.ssafy.lyricit.message.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessageHeaderAccessor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Controller;

import com.ssafy.lyricit.message.dto.MessageRequestDto;
import com.ssafy.lyricit.message.dto.MessageResponseDto;
import com.ssafy.lyricit.message.service.MessageService;

import lombok.RequiredArgsConstructor;

@Controller
@RequiredArgsConstructor
public class MessageController {
	private final MessageService messageService;
	private final SimpMessagingTemplate template; //특정 Broker로 메세지를 전달
	private final Logger log = LoggerFactory.getLogger(getClass());

	// Client가 SEND할 수 있는 경로
	// stompConfig에서 설정한 applicationDestinationPrefixes와 @MessageMapping 경로가 병합됨
	// "/pub/chat/enter"
	@MessageMapping("/chat/enter")
	public void enter(MessageRequestDto messageRequest, SimpMessageHeaderAccessor headerAccessor) {
		String memberId = (String)headerAccessor.getSessionAttributes().get("memberId");
		MessageResponseDto messageResponse = messageService.enter(memberId, messageRequest);
		log.info("{} 입장", messageResponse.nickname());
		template.convertAndSend("/sub/chat/room/" + messageResponse.roomNumber(), messageResponse);
	}

	@MessageMapping("/chat/exit")
	public void exit(MessageRequestDto messageRequest, SimpMessageHeaderAccessor headerAccessor) {
		String memberId = (String)headerAccessor.getSessionAttributes().get("memberId");
		MessageResponseDto messageResponse = messageService.exit(memberId, messageRequest);
		log.info("{} 퇴장", messageResponse.nickname());
		template.convertAndSend("/sub/chat/room/" + messageResponse.roomNumber(), messageResponse);
	}

	@MessageMapping("/chat/message")
	public void chat(MessageRequestDto messageRequest, SimpMessageHeaderAccessor headerAccessor) {
		String memberId = (String)headerAccessor.getSessionAttributes().get("memberId");
		MessageResponseDto messageResponse = messageService.chat(memberId, messageRequest);
		log.info("방번호 : {} \n {} : {} - {}",
			messageResponse.roomNumber(), messageResponse.nickname(),
			messageResponse.content(), messageResponse.time());
		template.convertAndSend("/sub/chat/room/" + messageResponse.roomNumber(), messageResponse);
	}
}
