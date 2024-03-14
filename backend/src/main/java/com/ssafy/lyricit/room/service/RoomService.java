package com.ssafy.lyricit.room.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.room.domain.Room;
import com.ssafy.lyricit.room.repository.RoomRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
// @Transactional
public class RoomService {
	private final RoomRepository roomRepository;

	public List<Room> findAllRooms() {
		return roomRepository.findAll();
	}

	public Room createRoom(String name) {
		return roomRepository.createRoom(name);
	}

	public Room findRoomById(Long id) {
		return roomRepository.findByRoomNumber(id);
	}
}
