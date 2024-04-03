package com.ssafy.lyricit.dictionary.repository;

import java.util.List;

import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.data.jpa.repository.Query;

import com.ssafy.lyricit.common.documents.TrackDocument;

public interface DictionaryRepository extends ElasticsearchRepository<TrackDocument, String> {

	@Query("{\"match\": {\"lyrics\": \"?0\"}}")
	List<TrackDocument> findByLyrics(String keyword);

}
