package com.ssafy.lyricit.common;

import java.io.Serializable;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class GlobalEventResponse<T> implements Serializable {
	private String type;
	private T data;
}
