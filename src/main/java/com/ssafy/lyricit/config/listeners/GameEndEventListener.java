package com.ssafy.lyricit.config.listeners;

import static com.ssafy.lyricit.common.type.EventType.*;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.config.events.GameEndEvent;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.service.RoomService;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class GameEndEventListener {
	private final RoomService roomService;
	private final MessagePublisher messagePublisher;

	@EventListener
	public void handleGameEndEvent(GameEndEvent event) {
		String roomNumber = event.getRoomNumber();
		RoomDto roomDto = roomService.validateRoom(roomNumber);
		roomDto.setIsPlaying(false);

		roomDto.getMembers().stream()
			.filter(member -> !member.getMember().memberId().equals(roomDto.getLeaderId()))
			.forEach(member -> member.setIsReady(false));

		roomService.updateRoom(roomNumber, roomDto);
		messagePublisher.publishRoomToLounge(ROOM_UPDATED.name(), roomDto);
	}
}
