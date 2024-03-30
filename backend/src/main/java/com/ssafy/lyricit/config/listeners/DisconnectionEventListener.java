package com.ssafy.lyricit.config.listeners;

import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.config.ChannelInboundInterceptor;
import com.ssafy.lyricit.config.events.DisconnectionEvent;

@Component
public class DisconnectionEventListener implements ApplicationListener<DisconnectionEvent> {
	private final ChannelInboundInterceptor channelInboundInterceptor;

	public DisconnectionEventListener(ChannelInboundInterceptor channelInboundInterceptor) {
		this.channelInboundInterceptor = channelInboundInterceptor;
	}

	@Override
	public void onApplicationEvent(DisconnectionEvent event) {
		channelInboundInterceptor.disconnect(event.getHeader());
	}
}
