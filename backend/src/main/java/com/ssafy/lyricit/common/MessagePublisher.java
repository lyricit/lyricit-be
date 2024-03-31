package com.ssafy.lyricit.common;

import static com.ssafy.lyricit.common.type.EndPointConstant.*;
import static com.ssafy.lyricit.common.type.EventType.*;

import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.chat.dto.LoungeChatResponseDto;
import com.ssafy.lyricit.chat.dto.RoomChatRequestDto;
import com.ssafy.lyricit.chat.dto.RoomChatResponseDto;
import com.ssafy.lyricit.chat.dto.GameChatResponseDto;
import com.ssafy.lyricit.member.dto.MemberOnlineDto;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class MessagePublisher {
	private final SimpMessagingTemplate template;

	public void publishOnlineMemberToLounge(MemberOnlineDto member, boolean isOnline) {
		template.convertAndSend(SUB_LOUNGE.getValue(),
			GlobalEventResponse.builder()
				.type(isOnline ? ONLINE.name() : OFFLINE.name())
				.data(member)
				.build()
		);
	}

	public void publishInOutMessageToRoom(boolean isIn, String roomNumber, String nickname) {
		String endPoint = isIn ? PUB_ENTER.getValue() : PUB_EXIT.getValue();
		template.convertAndSend(endPoint,// -> chat controller
			RoomChatRequestDto.builder()
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
				.build()
		);
	}

	public void publishMemberToRoom(String type, String roomNumber, Object member) {
		template.convertAndSend(SUB_ROOM.getValue() + roomNumber,
			GlobalEventResponse.builder()
				.type(type)
				.data(member)
				.build()
		);
	}

	public void publishLeaderChangedToRoom(String roomNumber, String leaderId) {
		template.convertAndSend(SUB_ROOM.getValue() + roomNumber,
			GlobalEventResponse.builder()
				.type(LEADER_CHANGED.name())
				.data(leaderId)
				.build()
		);
	}

	public void publishMessageToLounge(LoungeChatResponseDto response) {
		template.convertAndSend(SUB_LOUNGE.getValue(),
			GlobalEventResponse.builder()
				.type(LOUNGE_MESSAGE.name())
				.data(response)
				.build()
		);
	}

	public void publishMessageToRoom(RoomChatResponseDto response) {
		template.convertAndSend(SUB_ROOM.getValue() + response.roomNumber(),
			GlobalEventResponse.builder()
				.type(ROOM_MESSAGE.name())
				.data(response)
				.build()
		);
	}

	public void publishGameToRoom(String type, String roomNumber, Object gameInfo) {
		template.convertAndSend(SUB_ROOM.getValue() + roomNumber,
			GlobalEventResponse.builder()
				.type(type)
				.data(gameInfo)
				.build()
		);
	}

	public void publishGameToRoom(String type, String roomNumber) {
		template.convertAndSend(SUB_ROOM.getValue() + roomNumber,
			GlobalEventResponse.builder()
				.type(type)
				.build()
		);
	}

	public void publishMessageToGame(String roomNumber, GameChatResponseDto response) {
		template.convertAndSend(SUB_ROOM.getValue() + roomNumber,
			GlobalEventResponse.builder()
				.type(GAME_MESSAGE.name())
				.data(response)
				.build()
		);
	}

}
