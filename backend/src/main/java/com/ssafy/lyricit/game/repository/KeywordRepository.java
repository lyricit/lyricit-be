package com.ssafy.lyricit.game.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.ssafy.lyricit.game.domain.Keyword;

public interface KeywordRepository extends JpaRepository<Keyword, Long> {
	@Query(nativeQuery = true, value = "SELECT * FROM keyword ORDER BY RAND() LIMIT 1")
	Keyword findRandomKeyword();
}

