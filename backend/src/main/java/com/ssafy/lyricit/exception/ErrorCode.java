package com.ssafy.lyricit.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum ErrorCode {
	// Member
	MEMBER_NOT_FOUND(404, "회원를 찾을 수 없습니다."),
	MEMBER_ID_NOT_FOUND(404, "회원 ID를 찾을 수 없습니다."),

	MESSAGE_NOT_FOUND(404, "메세지를 찾을 수 없습니다."),

	// Room
	ROOM_NOT_FOUND(404, "방을 찾을 수 없습니다."),
	ROOM_FULL(409, "방이 꽉 찼습니다."),
	PASSWORD_REQUIRED(401, "비밀번호가 필요합니다."),
	WRONG_PASSWORD(401, "비밀번호가 틀렸습니다."),


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
