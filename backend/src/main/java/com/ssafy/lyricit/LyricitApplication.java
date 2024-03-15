package com.ssafy.lyricit;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@SpringBootApplication
@EnableJpaAuditing
public class LyricitApplication {

	public static void main(String[] args) {
		SpringApplication.run(LyricitApplication.class, args);
	}

}
