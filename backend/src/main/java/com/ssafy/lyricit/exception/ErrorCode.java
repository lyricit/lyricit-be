package com.ssafy.lyricit.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum ErrorCode {
	// Websocket
	DESTINATION_NOT_FOUND(404, "목적지를 찾을 수 없습니다."),

	// Member
	MEMBER_NOT_FOUND(404, "회원를 찾을 수 없습니다."),
	MEMBER_ALREADY_EXIST(409, "이미 존재하는 회원입니다."),
	MEMBER_ID_NOT_FOUND(404, "회원 ID를 찾을 수 없습니다."),
	HEADER_NOT_SATISFIED(400, "헤더가 요구사항을 만족하지 못합니다."),

	// Message
	MESSAGE_NOT_FOUND(404, "메세지를 찾을 수 없습니다."),

	// Room
	ROOM_NOT_FOUND(404, "방을 찾을 수 없습니다."),
	ROOM_FULL(409, "방이 꽉 찼습니다."),
	PASSWORD_REQUIRED(401, "비밀번호가 필요합니다."),
	WRONG_PASSWORD(401, "비밀번호가 틀렸습니다."),
	LEADER_CANNOT_READY(403, "방장은 레디 상태를 변경할 수 없습니다."),

	// Game
	GAME_NOT_FOUND(404, "게임을 찾을 수 없습니다."),
	GAME_ALREADY_PLAYING(409, "이미 게임이 진행 중입니다."),
	NOT_LEADER(403, "방장이 아닙니다."),
	NOT_ALL_READY(409, "전원이 레디 상태여야 게임 시작이 가능합니다."),
	KEYWORD_NOT_FOUND(404, "키워드를 찾을 수 없습니다."),

	// ElasticSearch
	TRACK_NOT_FOUND(404, "조회하는 Track ID가 존재하지 않습니다."),
	// Round
	ROUND_NOT_FOUND(404, "라운드를 찾을 수 없습니다."),
	ROUND_START_FAIL(500, "라운드 시작에 실패했습니다."),
	ROUND_END_FAIL(500, "라운드 종료에 실패했습니다."),

	// Score
	SCORE_NOT_FOUND(404, "점수를 찾을 수 없습니다."),


	// === GLOBAL BASE ERROR CODE ===
	// 4xx: Client Errors
	BAD_REQUEST(400, "Bad Request"),
	UNAUTHORIZED(401, "Unauthorized"),
	FORBIDDEN(403, "Forbidden"),
	NOT_FOUND(404, "Not Found"),
	METHOD_NOT_ALLOWED(405, "Method Not Allowed"),
	CONFLICT(409, "Conflict"),

	// 5xx: Server Errors
	INTERNAL_SERVER_ERROR(500, "Internal Server Error"),
	NOT_IMPLEMENTED(501, "Not Implemented"),
	BAD_GATEWAY(502, "Bad Gateway"),
	SERVICE_UNAVAILABLE(503, "Service Unavailable"),
	GATEWAY_TIMEOUT(504, "Gateway Timeout"),
	HTTP_VERSION_NOT_SUPPORTED(505, "HTTP Version Not Supported");

	private final Integer errorCode;
	private final String errorMsg;
}
