package com.ssafy.lyricit.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;

import lombok.RequiredArgsConstructor;

@Configuration
@RequiredArgsConstructor
@EnableWebSocketMessageBroker// 웹 소켓 메시지를 다룰 수 있게 허용
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {
	private final WebSocketHandler chatHandler;

	@Override
	public void configureMessageBroker(MessageBrokerRegistry config) {
		config.enableSimpleBroker("/topic"); // 발행자가 /topic 경로로 메시지를 주면 구독자들에게 전달
		config.setApplicationDestinationPrefixes("/app"); // 발행자가 "/app"의 경로로 메시지를 주면 가공을 해서 구독자들에게 전달
	}

	@Override
	public void registerStompEndpoints(StompEndpointRegistry registry) {
		registry.addEndpoint("/ws").setAllowedOrigins("*").withSockJS(); // 커넥션 맺는 경로 설정
		// 만약 WebSocket을 사용할 수 없는 브라우저라면 sock js를 사용하도록 설정
	}
}
