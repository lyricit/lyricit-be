package com.ssafy.lyricit.common.type;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum EndPointConstant {
	PUB_ENTER("/pub/chat/enter"),
	PUB_EXIT("/pub/chat/exit"),
	SUB_LOUNGE("/sub/lounge"),
	SUB_ROOM("/sub/rooms/");

	private final String value;
}
