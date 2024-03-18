package com.ssafy.lyricit.config;

import java.util.Map;

import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.service.MemberService;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class ChannelInboundInterceptor implements ChannelInterceptor {
	private final MemberService memberService;

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
		String memberId = header.getFirstNativeHeader("memberId");

		MemberDto memberDto = createMemberDto(header);

		if (memberId == null) {
			memberId = memberService.join(memberDto);
		} else {
			memberId = memberService.login(memberId, memberDto);
		}

		attributes.put("memberId", memberId);
		header.setSessionAttributes(attributes);
	}

	private MemberDto createMemberDto(StompHeaderAccessor header) {
		return MemberDto.builder()
			.nickname(header.getFirstNativeHeader("nickname"))
			.deco(header.getFirstNativeHeader("deco"))
			.face(header.getFirstNativeHeader("face"))
			.decoColor(header.getFirstNativeHeader("decoColor"))
			.faceColor(header.getFirstNativeHeader("faceColor"))
			.build();
	}
}
