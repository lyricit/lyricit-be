package com.ssafy.lyricit.round.service;

import org.quartz.DisallowConcurrentExecution;
import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.ssafy.lyricit.game.dto.GameDto;

@DisallowConcurrentExecution
public class RoundStartJob implements Job, ApplicationContextAware {

	private ApplicationContext applicationContext;

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) {
		this.applicationContext = applicationContext;
	}

	@Override
	public void execute(JobExecutionContext context) throws JobExecutionException {
		JobDataMap dataMap = context.getJobDetail().getJobDataMap();
		String roomNumber = dataMap.getString("roomNumber");

		RoundService roundService = applicationContext.getBean(RoundService.class);
		GameDto gameDto = roundService.validateGame(roomNumber);
		roundService.initRound(roomNumber, gameDto);
	}
}
