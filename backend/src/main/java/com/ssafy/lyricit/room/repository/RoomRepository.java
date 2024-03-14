package com.ssafy.lyricit.room.repository;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.stereotype.Repository;

import com.ssafy.lyricit.room.domain.Room;

import jakarta.annotation.PostConstruct;

@Repository
public class RoomRepository {

	private Map<Long, Room> chatRooms;

	@PostConstruct
	private void init() {
		chatRooms = new ConcurrentHashMap<>();
	}

	public List<Room> findAll() {
		return new ArrayList<>(chatRooms.values());
	}

	public Room findByRoomNumber(Long roomNumber) {
		return chatRooms.get(roomNumber);
	}

	public Room createRoom(String name) {// roomDto
		Room room = Room.builder().name(name).build();
		chatRooms.put(findEmptyRoomNumber(), Room.builder().name(name).build());
		return room;
	}

	private Long findEmptyRoomNumber() {
		Long roomNumber = 1L;
		while (chatRooms.containsKey(roomNumber)) {
			roomNumber++;
		}
		return roomNumber;
	}
}
