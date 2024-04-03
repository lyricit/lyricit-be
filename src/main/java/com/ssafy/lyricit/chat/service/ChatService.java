package com.ssafy.lyricit.chat.service;

import static com.ssafy.lyricit.chat.domain.type.ChatConstant.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.ssafy.lyricit.chat.domain.Chat;
import com.ssafy.lyricit.chat.dto.LoungeChatRequestDto;
import com.ssafy.lyricit.chat.dto.LoungeChatResponseDto;
import com.ssafy.lyricit.chat.dto.RoomChatRequestDto;
import com.ssafy.lyricit.chat.dto.RoomChatResponseDto;
import com.ssafy.lyricit.common.MessagePublisher;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
// @Transactional
public class ChatService {
	private final MessagePublisher messagePublisher;
	private final Logger log = LoggerFactory.getLogger(getClass());

	public void sendEnterMessage(RoomChatRequestDto chatRequest) {
		Chat chat = Chat.builder()
			.nickname(SYSTEM.getValue())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.nickname() + ENTERED.getValue())
			.build();
		// todo: save message

		messagePublisher.publishMessageToRoom(chat.toRoomChatResponseDto());
		log.info("\n{} 입장", chat.getNickname());
	}

	public void sendExitMessage(RoomChatRequestDto chatRequest) {
		Chat chat = Chat.builder()
			.nickname(SYSTEM.getValue())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.nickname() + EXITED.getValue())
			.build();
		// todo: save message

		messagePublisher.publishMessageToRoom(chat.toRoomChatResponseDto());
		log.info("\n{} 퇴장", chat.getNickname());
	}

	public void sendLoungeChatMessage(LoungeChatRequestDto chatRequest) {
		Chat chat = Chat.builder()
			.nickname(chatRequest.nickname())
			.roomNumber(LOUNGE.getValue())
			.content(chatRequest.content())
			.build();
		//todo: save message

		LoungeChatResponseDto response = chat.toLoungeChatResponseDto();
		messagePublisher.publishMessageToLounge(response);
		log.info("\n채팅 : 라운지 \n{} : {}   ({})",
			response.nickname(), response.content(), response.time());
	}

	public void sendRoomChatMessage(RoomChatRequestDto chatRequest) {
		Chat chat = Chat.builder()
			.nickname(chatRequest.nickname())
			.roomNumber(chatRequest.roomNumber())
			.content(chatRequest.content())
			.build();
		// todo: save message

		RoomChatResponseDto response = chat.toRoomChatResponseDto();
		messagePublisher.publishMessageToRoom(response);
		log.info("\n채팅 : {}번방 \n{} : {}   ({})",
			response.roomNumber(), response.nickname(),
			response.content(), response.time());
	}
}
