package com.ssafy.lyricit.round.job;

import static com.ssafy.lyricit.game.constant.JobName.*;

import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RoundEndJob implements Job {
	@Autowired
	private RoundService roundService;

	public void execute(JobExecutionContext context) throws JobExecutionException {

		JobDataMap dataMap = context.getJobDetail().getJobDataMap();
		String roomNumber = dataMap.getString(ROOM_NUMBER.getValue());

		roundService.endRound(roomNumber);
	}
}
