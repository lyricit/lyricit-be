package com.ssafy.lyricit.member.aspect;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.messaging.Message;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.repository.MemberRepository;

import lombok.RequiredArgsConstructor;

@Aspect
@Component
@RequiredArgsConstructor
public class MemberIdCheckAspect {
	private MemberRepository memberRepository;

	@Before("@annotation(memberIdCheck)")
	public void checkMemberId(JoinPoint joinPoint, MemberIdCheck memberIdCheck) {
		Object arg = joinPoint.getArgs()[0];
		if (arg instanceof Message) {
			StompHeaderAccessor headerAccessor = StompHeaderAccessor.wrap((Message<?>) arg);
			String memberId = headerAccessor.getFirstNativeHeader("memberId");
			if (!memberRepository.existsById(memberId)) {
				throw new BaseException(MEMBER_NOT_FOUND);
			}
		}
	}
}
