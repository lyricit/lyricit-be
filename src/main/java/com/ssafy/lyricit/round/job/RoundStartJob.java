package com.ssafy.lyricit.round.job;

import static com.ssafy.lyricit.game.constant.JobName.*;

import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.round.service.RoundService;

@Component
public class RoundStartJob implements Job {
	@Autowired
	private RoundService roundService;

	@Override
	public void execute(JobExecutionContext context) throws JobExecutionException {
		JobDataMap dataMap = context.getJobDetail().getJobDataMap();
		String roomNumber = dataMap.getString(ROOM_NUMBER.getValue());
		GameDto gameDto = roundService.validateGame(roomNumber);
		roundService.initRound(roomNumber, gameDto);
	}
}

