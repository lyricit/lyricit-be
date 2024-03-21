package com.ssafy.lyricit.config;

import static com.ssafy.lyricit.common.type.HeaderConstant.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.Map;

import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.repository.MemberRepository;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class ChannelInboundInterceptor implements ChannelInterceptor {
	private final MemberRepository memberRepository;

	@Override
	public Message<?> preSend(Message<?> message, MessageChannel channel) {
		StompHeaderAccessor header = StompHeaderAccessor.wrap(message);
		if (StompCommand.CONNECT.equals(header.getCommand())) {
			handleConnectCommand(header);
		}
		return message;
	}

	private void handleConnectCommand(StompHeaderAccessor header) {
		Map<String, Object> attributes = header.getSessionAttributes();
		String memberId = header.getFirstNativeHeader(MEMBER_ID.getValue());

		if (memberId == null || memberId.isBlank() || !memberRepository.existsById(memberId)) {
			throw new BaseException(MEMBER_ID_NOT_FOUND);
		}

		attributes.put(MEMBER_ID.getValue(), memberId);
		header.setSessionAttributes(attributes);
	}
}
