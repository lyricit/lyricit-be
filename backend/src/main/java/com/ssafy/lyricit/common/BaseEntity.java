package com.ssafy.lyricit.common;

import java.time.LocalDateTime;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class BaseEntity {
	private UUID id;
	private LocalDateTime createdAt;
	private LocalDateTime updatedAt;

	public void generateUUID() {
		if (id == null) {
			id = UUID.randomUUID();
		}
	}

	public void setCreatedAtNow() {
		this.createdAt = LocalDateTime.now();
	}

	public void setUpdatedAtNow() {
		this.updatedAt = LocalDateTime.now();
	}
}
