package com.ssafy.lyricit.round.service;

import static com.ssafy.lyricit.common.type.EventType.*;
import static com.ssafy.lyricit.exception.ErrorCode.*;

import java.util.Date;
import java.util.List;
import java.util.Random;

import org.quartz.JobBuilder;
import org.quartz.JobDetail;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.ssafy.lyricit.common.MessagePublisher;
import com.ssafy.lyricit.exception.BaseException;
import com.ssafy.lyricit.game.domain.Keyword;
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.game.dto.HighlightDto;
import com.ssafy.lyricit.game.dto.ScoreDto;
import com.ssafy.lyricit.game.repository.KeywordRepository;
import com.ssafy.lyricit.room.dto.RoomDto;
import com.ssafy.lyricit.room.service.RoomService;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@Transactional
public class RoundService {
	private final RedisTemplate<String, GameDto> gameRedisTemplate;
	private final RoomService roomService;
	private final KeywordRepository keywordRepository;
	private final Scheduler scheduler;
	private final MessagePublisher messagePublisher;
	private static final Random random = new Random();
	private final Logger log = LoggerFactory.getLogger(this.getClass());

	// round start after 2 seconds
	public void addRoundSchedule(String roomNumber) throws SchedulerException {
		if (isGameEnd(roomNumber)) {
			endGame(roomNumber, validateGame(roomNumber));
			log.info("\n{}번 방 게임 종료! \n", roomNumber);
			return;
		}
		scheduleRoundStart(roomNumber);
		scheduleRoundEnd(roomNumber);
	}

	private void scheduleRoundStart(String roomNumber) throws SchedulerException {
		JobDetail startJob = JobBuilder.newJob(RoundStartJob.class)
			.withIdentity(roomNumber, "roundJobs")
			.usingJobData("roomNumber", roomNumber)
			.build();
		Date startTime = new Date(System.currentTimeMillis() + 2000); // 2 seconds

		Trigger trigger = TriggerBuilder.newTrigger()
			.withIdentity(roomNumber, "roundTriggers")
			.startAt(startTime)
			.forJob(startJob)
			.build();

		scheduler.scheduleJob(startJob, trigger);
	}

	private void scheduleRoundEnd(String roomNumber) throws SchedulerException {
		JobDetail endJob = JobBuilder.newJob(RoundEndJob.class)
			.withIdentity(roomNumber, "roundJobs")
			.usingJobData("roomNumber", roomNumber)
			.build();
		Date startTime = new Date(System.currentTimeMillis() +
			validateGame(roomNumber).getRoundTime() * 1000);

		Trigger trigger = TriggerBuilder.newTrigger()
			.withIdentity(roomNumber, "roundTriggers")
			.startAt(startTime)
			.forJob(endJob)
			.build();

		scheduler.scheduleJob(endJob, trigger);
	}

	public void initRound(String roomNumber, GameDto gameDto) {
		Keyword keyword = getRandomKeyword();
		gameDto.getCorrectMembers().clear();

		// add round
		gameDto = gameDto.toBuilder()
			.currentRound(gameDto.getCurrentRound() + 1)
			.keyword(keyword.getWord())
			.highlightInfo(HighlightDto.create())
			.build();

		// update
		gameRedisTemplate.opsForValue().set(roomNumber, gameDto);

		// pub to room
		messagePublisher.publishGameToRoom(ROUND_STARTED.name(), roomNumber, gameDto.toRoundDto());
	}

	// cancel round if every member correct -> next round
	public void cancelScheduledJobs(String roomNumber) throws SchedulerException {
		JobKey startJobKey = JobKey.jobKey(roomNumber, "roundJobs");
		if (scheduler.checkExists(startJobKey)) {
			scheduler.deleteJob(startJobKey);// delete round start job
		}

		JobKey endJobKey = JobKey.jobKey(roomNumber, "roundJobs");
		if (scheduler.checkExists(endJobKey)) {
			scheduler.deleteJob(endJobKey);// delete round end job
		}

		messagePublisher.publishGameToRoom(ROUND_ENDED.name(), roomNumber);
		addRoundSchedule(roomNumber);// schedule next round
	}

	// end round if roundTime is over -> next round
	public void endRound(String roomNumber) {
		try {
			JobKey jobKey = JobKey.jobKey(roomNumber, "roundJobs");

			if (scheduler.checkExists(jobKey)) {
				scheduler.deleteJob(jobKey);
			}
			messagePublisher.publishGameToRoom(ROUND_ENDED.name(), roomNumber);
			addRoundSchedule(roomNumber);// schedule next round
		} catch (SchedulerException e) {
			log.error("Failed to end round for room: " + roomNumber, e);
		}
	}

	public void endGame(String roomNumber, GameDto gameDto) {
		List<ScoreDto> sortedMembers = gameDto.getMembers().stream()
			.sorted((a, b) -> Long.compare(b.getScore(), a.getScore()))
			.toList();

		messagePublisher.publishGameToRoom(GAME_ENDED.name(), roomNumber, sortedMembers);
		gameRedisTemplate.delete(roomNumber);
	}

	// get gameDto from db if exists
	public GameDto validateGame(String roomNumber) {
		if (Boolean.FALSE.equals(gameRedisTemplate.hasKey(roomNumber))) {
			throw new BaseException(GAME_NOT_FOUND);
		}
		return gameRedisTemplate.opsForValue().get(roomNumber);
	}

	public boolean isGameEnd(String roomNumber) {
		RoomDto roomDto = roomService.validateRoom(roomNumber);
		GameDto gameDto = validateGame(roomNumber);
		return gameDto.getCurrentRound() >= roomDto.getRoundLimit();
	}

	// get random keyword from db
	private Keyword getRandomKeyword() {
		return keywordRepository.findById(random.nextLong(keywordRepository.count()))
			.orElseThrow(() -> new BaseException(KEYWORD_NOT_FOUND));
	}
}
