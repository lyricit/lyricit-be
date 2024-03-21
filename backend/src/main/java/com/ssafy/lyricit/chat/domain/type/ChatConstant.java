package com.ssafy.lyricit.chat.domain.type;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ChatConstant {
	// system nickname
	SYSTEM("SYSTEM"),
	LOUNGE("LOUNGE"),

	// in & out
	ENTERED("님이 입장하셨습니다."),
	EXITED("님이 퇴장하셨습니다."),

	// room
	NOT_READY("님이 준비완료를 하지 않았습니다. READY 버튼을 눌러주세요!"),

	// game
	QUESTION("님이 입력한 가사가 있어요! 제한 시간 안에 제목과 가수를 입력하세요!"),
	CORRECT("님이 정답을 맞추셨습니다! 모두 축하해주세요!");

	private final String value;
}
