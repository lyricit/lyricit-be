package com.ssafy.lyricit.common.type;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum RedisDatabaseType {
	MEMBER_DB_IDX,
	ROOM_DB_IDX,
	GAME_DB_IDX
}
