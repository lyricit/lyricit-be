package com.ssafy.lyricit.config;

import org.quartz.Scheduler;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;

@Configuration
public class SchedulerConfig {
	@Bean
	public Scheduler scheduler(ApplicationContext applicationContext) throws Exception {
		AutowiringSpringBeanJobFactory jobFactory = new AutowiringSpringBeanJobFactory();
		jobFactory.setApplicationContext(applicationContext);

		SchedulerFactoryBean factory = new SchedulerFactoryBean();
		factory.setJobFactory(jobFactory);
		factory.afterPropertiesSet();

		Scheduler scheduler = factory.getScheduler();
		scheduler.start();
		return scheduler;
	}
}
