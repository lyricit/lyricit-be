package com.ssafy.lyricit.config.events;

import org.springframework.context.ApplicationEvent;

import com.ssafy.lyricit.member.dto.MemberOnlineDto;

import lombok.Getter;

@Getter
public class LoungeEvent extends ApplicationEvent {
	private final MemberOnlineDto memberOnlineDto;
	private final boolean isOnline;

	public LoungeEvent(Object source, MemberOnlineDto memberOnlineDto, boolean isOnline) {
		super(source);
		this.memberOnlineDto = memberOnlineDto;
		this.isOnline = isOnline;
	}
}
