package com.ssafy.lyricit.member.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.ssafy.lyricit.member.domain.Member;

@Repository
public interface MemberRepository extends JpaRepository<Member, String> {
}
