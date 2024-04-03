package com.ssafy.lyricit.keyword.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.ssafy.lyricit.keyword.domain.Keyword;

@Repository
public interface KeywordRepository extends JpaRepository<Keyword, Long> {
}

