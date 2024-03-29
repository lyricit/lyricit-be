package com.ssafy.lyricit.config.events;

import org.springframework.context.ApplicationEvent;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;

import lombok.Getter;

@Getter
public class DisconnectionEvent extends ApplicationEvent {
	private final StompHeaderAccessor header;

	public DisconnectionEvent(Object source, StompHeaderAccessor header) {
		super(source);
		this.header = header;
	}
}
