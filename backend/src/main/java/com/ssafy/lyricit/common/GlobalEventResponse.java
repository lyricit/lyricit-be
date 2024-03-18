package com.ssafy.lyricit.common;

import lombok.Builder;

@Builder
public class GlobalEventResponse<T> {
	private String type;
	private T data;
}
