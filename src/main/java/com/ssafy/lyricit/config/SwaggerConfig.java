package com.ssafy.lyricit.config;

import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.servers.Server;
import lombok.RequiredArgsConstructor;

@OpenAPIDefinition(
	info = @Info(title = "LYRIC:IT (리릭잇)",
		description = "LYRIC:IT api document",
		version = "v1"),
	servers = {@Server(url = "https://api-dev.lyricit.site", description = "Default Server URL")})
@RequiredArgsConstructor
@Configuration
public class SwaggerConfig {

	@Bean
	public GroupedOpenApi chatOpenApi() {
		String[] paths = {"/v1/**", "/ws/**", "/ping"};

		return GroupedOpenApi.builder()
			.group("LYRIC:IT API v1")
			.pathsToMatch(paths)
			.build();
	}
}
