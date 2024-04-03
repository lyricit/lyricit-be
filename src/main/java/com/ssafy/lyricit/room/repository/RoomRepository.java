package com.ssafy.lyricit.room.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.ssafy.lyricit.room.domain.Room;

@Repository
public interface RoomRepository extends JpaRepository<Room, String> {
}
