package com.ssafy.lyricit.config.listeners;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.config.events.LoungeEvent;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class LoungeEventListener {
	private final MessagePublisher messagePublisher;

	@EventListener
	public void handleLoungeEvent(LoungeEvent event) {
		messagePublisher.publishOnlineMemberToLounge(event.getMemberOnlineDto(), event.isOnline());
	}
}
