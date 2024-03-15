package com.ssafy.lyricit.message.service;

import java.time.LocalDateTime;

import org.springframework.stereotype.Service;

import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.repository.MemberRepository;
import com.ssafy.lyricit.message.domain.Message;
import com.ssafy.lyricit.message.dto.MessageRequestDto;
import com.ssafy.lyricit.message.dto.MessageResponseDto;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
// @Transactional
public class MessageService {
	private final MemberRepository memberRepository;

	public MessageResponseDto enter(String memberId, MessageRequestDto messageRequest) {
		Member member = memberRepository.findById(memberId).get();
		String nickname = member.getNickname();
		Long roomNumber = messageRequest.roomNumber();

		Message message = Message.builder()// request to entity
			.nickname(nickname)
			.roomNumber(roomNumber)
			.content(nickname + "님이 입장하셨습니다.")
			.time(LocalDateTime.now())
			.build();

		return message.toResponseDto();// entity to response
	}

	public MessageResponseDto exit(String memberId, MessageRequestDto messageRequest) {
		Member member = memberRepository.findById(memberId).get();
		String nickname = member.getNickname();
		Long roomNumber = messageRequest.roomNumber();

		Message message = Message.builder()// request to entity
			.nickname(nickname)
			.roomNumber(roomNumber)
			.content(nickname + "님이 퇴장하셨습니다.")
			.time(LocalDateTime.now())
			.build();

		return message.toResponseDto();// entity to response
	}

	public MessageResponseDto chat(String memberId, MessageRequestDto messageRequest) {
		Member member = memberRepository.findById(memberId).get();
		String nickname = member.getNickname();
		Long roomNumber = messageRequest.roomNumber();
		String content = messageRequest.content();

		Message message = Message.builder()// request to entity
			.nickname(nickname)
			.roomNumber(roomNumber)
			.content(content)
			.time(LocalDateTime.now())
			.build();

		return message.toResponseDto();// entity to response
	}
}
