package com.ssafy.lyricit.game.domain;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "keyword")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Keyword {
	@Id
	private Long id;
	private String word;
	private Long frequency;

}
