package com.ssafy.lyricit;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableJpaAuditing
@EnableAsync(proxyTargetClass = true)
public class LyricitApplication {

	public static void main(String[] args) {
		SpringApplication.run(LyricitApplication.class, args);
	}

}
