package com.ssafy.lyricit.room.service;

import static com.ssafy.lyricit.common.type.EventType.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.member.domain.Member;
import com.ssafy.lyricit.member.dto.MemberDto;
import com.ssafy.lyricit.member.dto.MemberInGameDto;
import com.ssafy.lyricit.member.repository.MemberRepository;
import com.ssafy.lyricit.room.domain.Room;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.dto.RoomInsideDto;
import com.ssafy.lyricit.room.dto.RoomOutsideDto;
import com.ssafy.lyricit.room.dto.RoomPasswordDto;
import com.ssafy.lyricit.room.dto.RoomRequestDto;
import com.ssafy.lyricit.room.repository.RoomRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class RoomService {
	private final RedisTemplate<String, RoomDto> roomRedisTemplate;
	private final RoomRepository roomRepository;
	private final MemberRepository memberRepository;
	private final MessagePublisher messagePublisher;
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

		RoomOutsideDto roomOutsideDto = roomDto.toOutsideDto(roomNumber);

		// pub to lounge
		messagePublisher.publishRoomToLounge(ROOM_CREATED.name(), roomOutsideDto);
		return roomNumber;
	}

	public RoomInsideDto enterRoom(String memberId, String roomNumber, RoomPasswordDto roomPasswordDto) {
		RoomDto roomDto = validateRoom(roomNumber);

		// check member already in room
		if (roomDto.getMembers().stream().anyMatch(m -> m.getMember().memberId().equals(memberId))) {
			throw new BaseException(MEMBER_ALREADY_EXIST);
		}

		// check password
		if (!memberId.equals(roomDto.getLeaderId()) && !roomDto.getPassword().isBlank()) {
			if (roomPasswordDto == null || roomPasswordDto.password().isBlank()) {// password not given
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
		MemberInGameDto memberInGameDto = memberRepository.findById(memberId)
			.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND))
			.toInGameDto();

		// add member to room
		roomDto.getMembers().add(memberInGameDto);
		roomDto.setPlayerCount(roomDto.getPlayerCount() + 1);

		publishRoom(true, roomNumber, roomDto, memberInGameDto, false);

		return roomDto.toInsideDto(roomNumber);
	}

	public void exitRoom(String memberId, String roomNumber) {
		if (roomNumber.equals("0")) {
			return;
		}
		RoomDto roomDto = validateRoom(roomNumber);

		// remove member in room if exist
		MemberInGameDto memberInGameDto = roomDto.getMembers().stream()
			.filter(m -> m.getMember().memberId().equals(memberId))
			.findFirst()
			.orElseThrow(() -> new BaseException(MEMBER_NOT_FOUND));

		roomDto.getMembers().remove(memberInGameDto);

		// update player count
		roomDto.setPlayerCount((long)roomDto.getMembers().size());

		// if room empty
		if (roomDto.getPlayerCount() == 0) {
			deleteRoom(roomNumber, roomDto);
			return;
		}

		boolean isLeaderChanged = false;
		// leader change
		if (roomDto.getLeaderId().equals(memberId)) { // if leader exited
			roomDto.setLeaderId(roomDto.getMembers().get(0).getMember().memberId()); // next member get leader
			// isReady false for leader
			roomDto.getMembers().get(0).setIsReady(false);
			isLeaderChanged = true;
		}

		publishRoom(false, roomNumber, roomDto, memberInGameDto, isLeaderChanged);
	}

	private void deleteRoom(String roomNumber, RoomDto roomDto) {
		roomRedisTemplate.delete(roomNumber);
		roomRepository.findById(roomDto.getRoomId())
			.orElseThrow(() -> new BaseException(ROOM_NOT_FOUND))
			.setDeleted(true);// soft deletion

		// pub
		messagePublisher.publishRoomToLounge(ROOM_DELETED.name(), roomNumber);
		log.info("\n [방 삭제 완료] \n {}", roomNumber);
	}

	private void publishRoom(boolean isIn, String roomNumber, RoomDto roomDto, MemberInGameDto memberInGameDto,
		boolean isLeaderChanged) {
		String type = isIn ? MEMBER_IN.name() : MEMBER_OUT.name();

		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);// update

		if (isIn) {
			log.info("\n [방 입장 완료] \n {}", memberInGameDto.getMember().nickname());
		} else {
			log.info("\n [방 퇴장 완료] \n {}", memberInGameDto.getMember().nickname());
		}

		// pub
		messagePublisher.publishRoomToLounge(ROOM_UPDATED.name(), roomDto.toOutsideDto(roomNumber));
		messagePublisher.publishMemberToRoom(type, roomNumber, memberInGameDto);
		messagePublisher.publishInOutMessageToRoom(isIn, roomNumber, memberInGameDto.getMember().nickname());

		if (isLeaderChanged) {
			if (roomDto.getMembers().get(0).getIsReady()) {
				ready(roomDto.getLeaderId(), roomNumber);
			}

			messagePublisher.publishLeaderChangedToRoom(roomNumber, roomDto.getLeaderId());
		}
	}

	public void ready(String memberId, String roomNumber) {
		RoomDto roomDto = validateRoom(roomNumber);

		// find the member and set isReady to opposite
		for (MemberInGameDto memberInGameDto : roomDto.getMembers()) {
			MemberDto member = memberInGameDto.getMember();
			if (member.memberId().equals(memberId)) {
				memberInGameDto.setIsReady(!memberInGameDto.getIsReady());// reverse
				log.info("\n [레디 완료] \n {}", memberInGameDto.getMember().nickname());
				break;
			}
		}

		// update to Redis
		roomRedisTemplate.opsForValue().set(roomNumber, roomDto);

		// pub
		messagePublisher.publishMemberToRoom(MEMBER_READY.name(), roomNumber, memberId);
	}

	public List<RoomOutsideDto> readAllRooms() {// 라운지 접근 시 단 한번 호출
		Set<String> roomNumbers = roomRedisTemplate.keys("*"); // get all keys
		List<RoomOutsideDto> roomOutsideDtoList = new ArrayList<>();

		if (roomNumbers != null) {
			for (String roomNumber : roomNumbers) {
				RoomDto roomDto = roomRedisTemplate.opsForValue().get(roomNumber); // get value by key
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
		RoomOutsideDto roomOutsideDto = validateRoom(roomNumber).toOutsideDto(roomNumber);
		log.info("\n [방 조회 결과] \n {}", roomOutsideDto);
		return roomOutsideDto;
	}

	// get roomDto from db if exists
	public RoomDto validateRoom(String roomNumber) {
		if (Boolean.FALSE.equals(roomRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(ROOM_NOT_FOUND);
		}
		return roomRedisTemplate.opsForValue().get(roomNumber);
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
}
