-- | archive_message @ application

-- [ drop-table-archive_message
DROP TABLE archive_message;
-- ]

-- [ create-table-archive_message
CREATE TABLE archive_message (
	id			INTEGER UNSIGNED		PRIMARY KEY AUTO_INCREMENT,
	headers			TEXT				NOT NULL,
	body			LONGTEXT			NOT NULL,
	message_ptr		INTEGER UNSIGNED		NOT NULL
) ENGINE = MyISAM;
-- ]

-- [ create-index-archive_message_by_headers
CREATE FULLTEXT INDEX archive_message_by_headers ON archive_message (headers);
-- ]

-- [ create-index-archive_message_by_body
CREATE FULLTEXT INDEX archive_message_by_body ON archive_message (body);
-- ]

-- [ create-index-archive_message_by_headers_and_body
CREATE FULLTEXT INDEX archive_message_by_headers_and_body ON archive_message (headers, body);
-- ]

