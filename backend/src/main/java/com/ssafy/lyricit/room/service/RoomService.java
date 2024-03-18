package com.ssafy.lyricit.room.service;

import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.repository.MemberRepository;
import com.ssafy.lyricit.room.domain.Room;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.dto.RoomInsideDto;
import com.ssafy.lyricit.room.dto.RoomOutsideDto;
import com.ssafy.lyricit.room.dto.RoomRequestDto;
import com.ssafy.lyricit.room.repository.RoomRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class RoomService {
	private final RedisTemplate<String, Object> roomRedisTemplate;
	private final RoomRepository roomRepository;
	private final MemberRepository memberRepository;
	private final SimpMessagingTemplate template; // 특정 Broker로 메세지를 전달

	public void createRoom(String memberId, RoomRequestDto roomRequest) {
		// to mysql
		Member member = memberRepository.findById(memberId).orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
		Room room = roomRequest.toEntity(member);
		roomRepository.save(room);// mysql에 저장

		// to redis
		RoomDto roomDto = room.toDto(member.toInGameDto());// for redis
		String roomNumber = findEmptyRoomNumber();// 기반으로 redis에 저장
		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);// redis에 저장

		// pub
		RoomInsideDto roomInsideDto = roomDto.toInsideDto(roomNumber);
		RoomOutsideDto roomOutsideDto = roomDto.toOutsideDto(roomNumber);

		// pub to room
		template.convertAndSend("/sub/rooms" + roomInsideDto.roomNumber(), roomInsideDto);

		// pub to lounge
		template.convertAndSend("/sub/lounge", roomOutsideDto); // 새로 추가된 방만 publish
	}

	public void readAllRooms() {// 라운지 접근 시 단 한번 호출
		Set<String> roomNumbers = roomRedisTemplate.keys("*"); // get all keys
		List<RoomOutsideDto> roomOutsideDtoList = new ArrayList<>();

		if (roomNumbers != null) {
			for (String roomNumber : roomNumbers) {
				RoomDto roomDto = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber); // get value by key
				if (roomDto != null) {
					RoomOutsideDto roomOutsideDto = roomDto.toOutsideDto(roomNumber);
					roomOutsideDtoList.add(roomOutsideDto);
				}
			}
		}

		template.convertAndSend("/sub/lounge", roomOutsideDtoList);
	}

	public void readRoomByRoomNumber(String roomNumber) {
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(ROOM_NOT_FOUND);
		}

		RoomOutsideDto roomOutsideDto =
			((RoomDto)roomRedisTemplate.opsForValue().get(roomNumber)).toOutsideDto(roomNumber);

		template.convertAndSend("/sub/rooms/" + roomNumber, roomOutsideDto);// 본인한테만 보내도록 설정
	}

	// 방 삭제 메서드

	private String findEmptyRoomNumber() {
		long roomNumber = 1L;
		String roomNumberStr;
		do {
			roomNumberStr = Long.toString(roomNumber);
			roomNumber++;
		} while (Boolean.TRUE.equals(roomRedisTemplate.hasKey(roomNumberStr)));
		return roomNumberStr;
	}
}
