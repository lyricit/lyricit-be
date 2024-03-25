package com.ssafy.lyricit.config;

import static com.ssafy.lyricit.common.type.HeaderConstant.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.Map;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.config.events.LoungeEvent;
import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.dto.MemberOnlineDto;
import com.ssafy.lyricit.member.repository.MemberRepository;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class ChannelInboundInterceptor implements ChannelInterceptor {
	private final MemberRepository memberRepository;
	private final RedisTemplate<String, String> memberRedisTemplate;
	private final ApplicationEventPublisher eventPublisher;

	@Override
	public Message<?> preSend(Message<?> message, MessageChannel channel) {
		StompHeaderAccessor header = StompHeaderAccessor.wrap(message);
		if (StompCommand.CONNECT.equals(header.getCommand())) {
			// add online member to redis template
			String memberId = header.getFirstNativeHeader(MEMBER_ID.getValue());
			if (memberId == null || memberId.isBlank()) {
				throw new BaseException(MEMBER_ID_NOT_FOUND);
			}

			String nickname = memberRepository.findById(memberId)
				.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND)).getNickname();

			memberRedisTemplate.opsForValue().set(memberId, nickname);

			handleConnectCommand(header);

			// publish to lounge
			eventPublisher.publishEvent(new LoungeEvent(this,
				MemberOnlineDto.builder()
					.memberId(memberId)
					.nickname(nickname)
					.build()
				, true
			));
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

	@Override
	public void afterSendCompletion(Message<?> message, MessageChannel channel, boolean sent, Exception ex) {
		StompHeaderAccessor header = StompHeaderAccessor.wrap(message);
		if (StompCommand.DISCONNECT.equals(header.getCommand())) {
			// remove online member from redis template
			String memberId = header.getSessionAttributes().get(MEMBER_ID.getValue()).toString();
			if (memberId != null && !memberId.isBlank() && Boolean.TRUE.equals(memberRedisTemplate.hasKey(memberId))) {
				// return;
				String nickname = memberRedisTemplate.opsForValue().get(memberId);
				// remove from redis
				memberRedisTemplate.delete(memberId);

				// publish to lounge
				eventPublisher.publishEvent(new LoungeEvent(this,
					MemberOnlineDto.builder()
						.memberId(memberId)
						.nickname(nickname)
						.build()
					, false
				));

				return;
			}

			throw new BaseException(MEMBER_ID_NOT_FOUND);
		}
	}
}
