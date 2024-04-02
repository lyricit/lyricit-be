package com.ssafy.lyricit.config.listeners;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.config.events.HighlightCancelEvent;
import com.ssafy.lyricit.game.service.GameChatService;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class HighlightCancelEventListener {
	private final GameChatService gameChatService;

	@EventListener
	public void handleHighlightCancelEvent(HighlightCancelEvent event) {
		gameChatService.cancelHighlightTask(event.getRoomNumber());
	}
}
