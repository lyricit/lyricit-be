package com.ssafy.lyricit.config.events;

import org.springframework.context.ApplicationEvent;

import lombok.Getter;

@Getter
public class RoomEvent extends ApplicationEvent {
	private final String memberId;
	private final String roomNumber;

	public RoomEvent(Object source, String memberId, String roomNumber) {
		super(source);
		this.memberId = memberId;
		this.roomNumber = roomNumber;
	}
}
