package com.ssafy.lyricit.chat.controller;

import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.web.bind.annotation.RestController;

@RestController("/ws")
public class ChatController {
	@MessageMapping("/chat")
	@SendTo("/topic/chat")// /topic/chat을 구독하고있는 모든 subscriber에게 메시지를 전달
	public String chat(String message) {
		return message;
	}
}
