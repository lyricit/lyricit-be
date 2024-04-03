package com.ssafy.lyricit.config.events;

import org.springframework.context.ApplicationEvent;

import lombok.Getter;

@Getter
public class HighlightCancelEvent extends ApplicationEvent {
	private final String roomNumber;

	public HighlightCancelEvent(Object source, String roomNumber) {
		super(source);
		this.roomNumber = roomNumber;
	}
}
