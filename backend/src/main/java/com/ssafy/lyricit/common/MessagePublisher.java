package com.ssafy.lyricit.common;

import static com.ssafy.lyricit.common.type.EndPointConstant.*;

import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.chat.dto.ChatRequestDto;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class MessagePublisher {
	private final SimpMessagingTemplate template; // 특정 Broker로 메세지를 전달

	public void publishInOutMessageToRoom(boolean isIn, String roomNumber, String nickname) {
		String endPoint = isIn ? PUB_ENTER.getValue() : PUB_EXIT.getValue();
		template.convertAndSend(endPoint,// -> chat controller
			ChatRequestDto.builder()
				.roomNumber(roomNumber)
				.nickname(nickname)
				.content("")
				.build()
		);
	}

	public void publishRoomToLounge(String type, Object room) {
		template.convertAndSend(SUB_LOUNGE.getValue(),
			GlobalEventResponse.builder()
				.type(type)
				.data(room)
				.build());
	}

	public void publishMemberToRoom(String type, String roomNumber, Object member) {
		template.convertAndSend(SUB_ROOM.getValue() + roomNumber,
			GlobalEventResponse.builder()
				.type(type)
				.data(member)
				.build());
	}

	public void publishMessageToRoom(String roomNumber, Object message) {
		template.convertAndSend(SUB_ROOM.getValue() + roomNumber, message);
	}
}
