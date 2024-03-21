package com.ssafy.lyricit.chat.service;

import static com.ssafy.lyricit.chat.domain.type.ChatConstant.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.ssafy.lyricit.chat.domain.Chat;
import com.ssafy.lyricit.chat.dto.ChatRequestDto;
import com.ssafy.lyricit.chat.dto.ChatResponseDto;
import com.ssafy.lyricit.common.MessagePublisher;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
// @Transactional
public class ChatService {
	private final MessagePublisher messagePublisher;
	private final Logger log = LoggerFactory.getLogger(getClass());

	public void sendEnterMessage(ChatRequestDto chatRequest) {
		Chat chat = Chat.builder()
			.nickname(SYSTEM.getValue())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.nickname() + ENTERED.getValue())
			.build();
		// todo: save message

		messagePublisher.publishMessageToRoom(chatRequest.roomNumber(), chat.toResponseDto());
		log.info("\n{} 입장", chat.getNickname());
	}

	public void sendExitMessage(ChatRequestDto chatRequest) {
		Chat chat = Chat.builder()
			.nickname(SYSTEM.getValue())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.nickname() + EXITED.getValue())
			.build();
		// todo: save message

		messagePublisher.publishMessageToRoom(chatRequest.roomNumber(), chat.toResponseDto());
		log.info("\n{} 퇴장", chat.getNickname());
	}

	public void sendChatMessage(ChatRequestDto chatRequest) {
		Chat chat = Chat.builder()
			.nickname(chatRequest.nickname())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.content())
			.build();
		// todo: save message

		ChatResponseDto response = chat.toResponseDto();
		messagePublisher.publishMessageToRoom(chatRequest.roomNumber(), response);
		log.info("\n채팅 : {}번방 \n{} : {}   ({})",
			response.roomNumber(), response.nickname(),
			response.content(), response.time());
	}
}
