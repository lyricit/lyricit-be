package com.ssafy.lyricit.config;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import com.google.gson.Gson;
import com.ssafy.lyricit.chat.domain.ChatMessage;
import com.ssafy.lyricit.exception.BaseException;

import jakarta.annotation.PostConstruct;
import lombok.NonNull;

@Component
public class WebSocketHandler extends TextWebSocketHandler {
	private Map<Long, UUID> roomNumberKeyMap; // roomNumber -> UUID
	private Map<UUID, Set<WebSocketSession>> roomSessions; // UUID -> session set

	@PostConstruct
	public void init() {
		roomNumberKeyMap = new ConcurrentSkipListMap<>();
		roomSessions = new ConcurrentHashMap<>();
	}

	/**
	 * @param session : 새로운 session
	 * @throws Exception
	 *
	 * 연결이 되고나면 sessions에 있는 모든 session에게 "입장 메시지"를 publish
	 */
	@Override
	public void afterConnectionEstablished(WebSocketSession session) throws Exception {
		super.afterConnectionEstablished(session);
		Long roomNumber = Long.parseLong(session.getHandshakeHeaders().get("roomNumber").get(0));
		UUID key = roomNumberKeyMap.getOrDefault(roomNumber, UUID.randomUUID());
		String nickname = session.getHandshakeHeaders().get("nickname").get(0);

		Set<WebSocketSession> sessions = roomSessions.get(key); // exception 처리
		sessions.add(session);

		System.out.println("sessions = " + sessions.size());
		sessions.forEach(s -> { // 연결이 되고나면 sessions에 있는 모든 session에게 입장 메시지를 보냄 (pub)
			try {
				s.sendMessage(new TextMessage(nickname + "님께서 입장하셨습니다."));
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		});
	}

	/**
	 * @param session : 메시지를 보낸 session
	 * @param message : 보낸 메시지
	 *
	 * sessions에 있는 모든 session에게 "메시지"를 publish
	 */
	@Override
	protected void handleTextMessage(
		@NonNull WebSocketSession session,
		@NonNull TextMessage message) throws Exception {
		super.handleTextMessage(session, message);
		Gson gson = new Gson();
		String nickname = session.getHandshakeHeaders().get("nickname").get(0);
		Set<WebSocketSession> sessions = roomSessions.getOrDefault(getRoomKey(session), new HashSet<>());

		sessions.forEach(s -> {
			try {
				ChatMessage chatMessage = gson.fromJson(message.getPayload(), ChatMessage.class);
				s.sendMessage(
					new TextMessage(
						nickname + " : " + chatMessage.getContent() + "[" + chatMessage.getCreatedAt() + "]"));
			} catch (IOException e) {
				throw new BaseException(MESSAGE_NOT_FOUND); // 메세지 유효성 검증하기
			}
		});
	}

	/**
	 * @param session : 종료된 session
	 * @param status : 종료 상태
	 * @throws Exception
	 *
	 * sessions에 있는 모든 session에게 "퇴장 메시지"를 publish
	 */
	@Override
	public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull CloseStatus status) throws Exception {
		super.afterConnectionClosed(session, status);
		Set<WebSocketSession> sessions = roomSessions.get(getRoomKey(session)); // exception 처리

		sessions.remove(session); // set 에서 삭제
		if (sessions.isEmpty()) { // 방이 비었다면 방 삭제
			Long roomNumber = Long.parseLong(session.getHandshakeHeaders().get("roomNumber").get(0));
			roomSessions.remove(getRoomKey(session)); // 방 session 삭제
			roomNumberKeyMap.remove(roomNumber); // 방 번호 재사용을 위해 방 번호 삭제
			return;
		}

		System.out.println("session = " + sessions.size());
		String nickname = session.getHandshakeHeaders().get("nickname").get(0);
		sessions.forEach(s -> {
			try {
				s.sendMessage(new TextMessage(nickname + "님께서 퇴장하셨습니다."));
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		});
	}

	/**
	 * @return UUID
	 *
	 * session의 roomNumber를 통해 roomNumberKeyMap에서 UUID를 get
	 */
	private UUID getRoomKey(@NonNull WebSocketSession session) {
		Long roomNumber = Long.parseLong(session.getHandshakeHeaders().get("roomNumber").get(0));
		return roomNumberKeyMap.getOrDefault(roomNumber, UUID.randomUUID());
	}
}
