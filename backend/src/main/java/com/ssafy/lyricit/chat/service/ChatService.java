package com.ssafy.lyricit.chat.service;

import static com.ssafy.lyricit.chat.domain.type.ChatConstant.*;

import org.springframework.stereotype.Service;

import com.ssafy.lyricit.chat.domain.Chat;
import com.ssafy.lyricit.chat.dto.ChatRequestDto;
import com.ssafy.lyricit.chat.dto.ChatResponseDto;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
// @Transactional
public class ChatService {
	public ChatResponseDto createEnterMessage(ChatRequestDto chatRequest) {
		Chat chat = Chat.builder()// request to entity
			.nickname(SYSTEM.getValue())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.nickname() + ENTERED.getValue())
			.build();

		// todo: save message

		return chat.toResponseDto();// entity to response
	}

	public ChatResponseDto createExitMessage(ChatRequestDto chatRequest) {
		Chat chat = Chat.builder()// request to entity
			.nickname(SYSTEM.getValue())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.nickname() + EXITED.getValue())
			.build();

		// todo: save message

		return chat.toResponseDto();// entity to response
	}

	public ChatResponseDto createChatMessage(ChatRequestDto chatRequest) {
		Chat chat = Chat.builder()// request to entity
			.nickname(chatRequest.nickname())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.content())
			.build();

		// todo: save message

		return chat.toResponseDto();// entity to response
	}
}
