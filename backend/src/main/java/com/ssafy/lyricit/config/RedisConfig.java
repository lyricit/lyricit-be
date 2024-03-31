package com.ssafy.lyricit.config;

import static com.ssafy.lyricit.common.type.RedisDatabaseType.*;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import com.ssafy.lyricit.game.dto.GameDto;
import com.ssafy.lyricit.round.dto.RoundDto;
import com.ssafy.lyricit.room.dto.RoomDto;

@Configuration
@EnableTransactionManagement
public class RedisConfig {

	@Value("${spring.data.redis.host}")
	private String host;

	@Value("${spring.data.redis.password}")
	private String password;

	@Value("${spring.data.redis.port}")
	private int port;

	private LettuceConnectionFactory createLettuceConnectionFactory(int database) {
		RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
		redisStandaloneConfiguration.setHostName(host);
		redisStandaloneConfiguration.setPort(port);
		redisStandaloneConfiguration.setPassword(password);
		redisStandaloneConfiguration.setDatabase(database);

		LettuceConnectionFactory lettuceConnectionFactory = new LettuceConnectionFactory(redisStandaloneConfiguration);
		lettuceConnectionFactory.afterPropertiesSet();

		return lettuceConnectionFactory;
	}

	private <T> RedisTemplate<String, T> createRedisTemplate(int database, Class<T> clazz) {
		RedisTemplate<String, T> redisTemplate = new RedisTemplate<>();
		redisTemplate.setConnectionFactory(createLettuceConnectionFactory(database));
		redisTemplate.setKeySerializer(new StringRedisSerializer());

		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.registerModule(new JavaTimeModule());
		objectMapper.registerModule(new Jdk8Module());
		objectMapper.registerModule(new ParameterNamesModule());

		Jackson2JsonRedisSerializer<T> serializer = new Jackson2JsonRedisSerializer<>(clazz);
		serializer.setObjectMapper(objectMapper);
		redisTemplate.setValueSerializer(serializer);

		return redisTemplate;
	}

	@Bean
	public RedisTemplate<String, String> memberRedisTemplate() {
		return createRedisTemplate(MEMBER_DB_IDX.ordinal(), String.class);
	}

	@Bean
	public RedisTemplate<String, RoomDto> roomRedisTemplate() {
		return createRedisTemplate(ROOM_DB_IDX.ordinal(), RoomDto.class);
	}

	@Bean
	public RedisTemplate<String, GameDto> gameRedisTemplate() {
		return createRedisTemplate(GAME_DB_IDX.ordinal(), GameDto.class);
	}

	@Bean
	public RedisTemplate<String, RoundDto> roundRedisTemplate() {
		return createRedisTemplate(ROUND_DB_IDX.ordinal(), RoundDto.class);
	}
}
