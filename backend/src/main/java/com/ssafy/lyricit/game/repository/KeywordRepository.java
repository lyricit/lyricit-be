package com.ssafy.lyricit.game.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.ssafy.lyricit.game.domain.Keyword;

@Repository
public interface KeywordRepository extends JpaRepository<Keyword, Long> {
}

