package com.ssafy.lyricit.config.events;

import org.springframework.context.ApplicationEvent;

import lombok.Getter;

@Getter
public class GameEndEvent extends ApplicationEvent {
	private final String roomNumber;

	public GameEndEvent(Object source, String roomNumber) {
		super(source);
		this.roomNumber = roomNumber;
	}
}
