package com.ssafy.lyricit.room.service;

import static com.ssafy.lyricit.common.type.EventType.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.chat.dto.ChatRequestDto;
import com.ssafy.lyricit.common.GlobalEventResponse;
import com.ssafy.lyricit.common.type.EventType;
import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.member.repository.MemberRepository;
import com.ssafy.lyricit.room.domain.Room;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.dto.RoomOutsideDto;
import com.ssafy.lyricit.room.dto.RoomPasswordDto;
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
	private final SimpMessagingTemplate template;
	private final Logger log = LoggerFactory.getLogger(this.getClass());

	public String createRoom(String memberId, RoomRequestDto roomRequest) {
		// to mysql
		Member member = memberRepository.findById(memberId).orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));
		Room room = roomRequest.toEntity(member);
		roomRepository.save(room);

		// to redis
		RoomDto roomDto = room.toDto();
		String roomNumber = findEmptyRoomNumber();
		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);

		log.info("\n [방 생성 완료] \n== mysql 저장 ==\n {} \n", room);
		log.info("\n== redis 저장 == \n [{}번 방] \n {}", roomNumber, roomDto);

		// pub to lounge
		RoomOutsideDto roomOutsideDto = roomDto.toOutsideDto(roomNumber);
		template.convertAndSend("/sub/lounge",
			GlobalEventResponse.builder()
				.type(EventType.ROOM_CREATED.name())
				.data(roomOutsideDto)
				.build());

		return roomNumber;
	}

	public void enterRoom(String memberId, String roomNumber, RoomPasswordDto roomPasswordDto) {
		// check redis key
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(ROOM_NOT_FOUND);
		}

		RoomDto roomDto = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber);

		// check password
		if (!roomDto.getPassword().isBlank()) {
			if (roomPasswordDto == null) {// password not given
				throw new BaseException(PASSWORD_REQUIRED);
			} else if (!roomPasswordDto.password().equals(roomDto.getPassword())) {// password wrong
				throw new BaseException(WRONG_PASSWORD);
			}
		}

		// check room full
		if (roomDto.getMembers().size() >= roomDto.getPlayerLimit()) {
			throw new BaseException(ROOM_FULL);
		}

		// set new member for room
		MemberInGameDto newMember = memberRepository.findById(memberId)
			.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND))
			.toInGameDto();

		// add member to room
		roomDto.getMembers().add(newMember);
		roomDto.setPlayerCount(roomDto.getPlayerCount() + 1);
		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);// update

		log.info("\n [방 입장 완료] \n {}", newMember.getMember().nickname());

		// pub member to room
		template.convertAndSend("/sub/rooms/" + roomNumber,
			GlobalEventResponse.builder()
				.type(MEMBER_IN.name())
				.data(newMember)
				.build());

		// pub message to room
		template.convertAndSend("/pub/chat/enter",// -> chat controller
			ChatRequestDto.builder()
				.roomNumber(roomNumber)
				.nickname(newMember.getMember().nickname())
				.content("")
				.build()
		);
	}

	public void exitRoom(String memberId, String roomNumber) {
		// check redis key
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(ROOM_NOT_FOUND);
		}

		RoomDto roomDto = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber);

		// remove member from room
		MemberInGameDto memberInGameDto = memberRepository.findById(memberId)
			.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND))
			.toInGameDto();

		roomDto.getMembers().remove(memberInGameDto);
		roomDto.setPlayerCount(roomDto.getPlayerCount() - 1);

		// if room empty
		if (roomDto.getPlayerCount() == 0) {
			roomRedisTemplate.delete(roomNumber);
			roomRepository.findById(roomDto.getRoomId())
				.orElseThrow(() -> new BaseException(ROOM_NOT_FOUND))
				.setDeleted(true);// soft deletion

			template.convertAndSend("/sub/lounge",
				GlobalEventResponse.builder()
					.type(EventType.ROOM_DELETED.name())
					.data(roomNumber)
					.build());
			log.info("\n [방 삭제 완료] \n {}", roomNumber);
			return;
		}

		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);// update

		log.info("\n [방 퇴장 완료] \n {}", memberInGameDto.getMember().nickname());

		// pub member to room
		template.convertAndSend("/sub/rooms/" + roomNumber,
			GlobalEventResponse.builder()
				.type(MEMBER_OUT.name())
				.data(memberInGameDto)
				.build());

		// pub message to room
		template.convertAndSend("/pub/chat/exit",// -> chat controller
			ChatRequestDto.builder()
				.roomNumber(roomNumber)
				.nickname(memberInGameDto.getMember().nickname())
				.content("")
				.build()
		);
	}

	public List<RoomOutsideDto> readAllRooms() {// 라운지 접근 시 단 한번 호출
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

		log.info("\n [모든 방 조회 결과] \n {}", roomOutsideDtoList);
		return roomOutsideDtoList;
	}

	public RoomOutsideDto readRoomByRoomNumber(String roomNumber) {
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(ROOM_NOT_FOUND);
		}

		RoomOutsideDto roomOutsideDto =
			((RoomDto)roomRedisTemplate.opsForValue().get(roomNumber)).toOutsideDto(roomNumber);

		log.info("\n [방 조회 결과] \n {}", roomOutsideDto);
		return roomOutsideDto;
	}

	// 방 삭제 메서드
	public void deleteRoomByRoomNumber(String roomNumber) {
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(ROOM_NOT_FOUND);
		}
		RoomDto roomDto = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber); // todo: add null exception

		// delete from mysql
		Room room = roomRepository.findById(roomDto.getRoomId()).orElseThrow(() -> new BaseException(ROOM_NOT_FOUND));
		room.setDeleted(true); // soft deletion

		// delete from redis
		roomRedisTemplate.delete(roomNumber);

		log.info("\n [방 삭제 완료] : {}", roomNumber);

		// pub to lounge -> client do delete job
		template.convertAndSend("/sub/lounge",
			GlobalEventResponse.builder()
				.type(EventType.ROOM_DELETED.name())
				.data(roomNumber)
				.build());
	}

	private String findEmptyRoomNumber() {
		long roomNumber = 1L;
		String roomNumberStr;
		do {
			roomNumberStr = Long.toString(roomNumber);
			roomNumber++;
		} while (Boolean.TRUE.equals(roomRedisTemplate.hasKey(roomNumberStr)));
		return roomNumberStr;
	}

	public void ready(String memberId, String roomNumber) {
		// check redis key
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(BAD_REQUEST);
		}

		RoomDto roomDto = (RoomDto)roomRedisTemplate.opsForValue().get(roomNumber);

		// find the member and set isReady to opposite

		for (MemberInGameDto memberInGameDto : roomDto.getMembers()) {
			MemberDto member = memberInGameDto.member();
			if (member.memberId().equals(memberId)) {
				memberInGameDto.isReady() = !memberInGameDto.isReady();
				break;
			}
		}

		// update to Redis
		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);

		// pub to room
		template.convertAndSend("/sub/room/" + roomNumber,
			GlobalEventResponse.builder()
				.type(MEMBER_READY.name())
				.data(memberId)
				.build());
	}
}

