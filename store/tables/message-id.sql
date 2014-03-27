-- | message_id @ application

-- [ drop-table-message_id
DROP TABLE message_id;
-- ]

-- [ create-table-message_id
CREATE TABLE message_id (
	id			INTEGER UNSIGNED		PRIMARY KEY AUTO_INCREMENT,
	header			VARCHAR(255)	NOT NULL,
	message_ptr		INTEGER UNSIGNED		-- null = forward referenced message-id
) ENGINE = INNODB;
-- ]

-- [ drop-index-message_id_by_message_ptr
DROP INDEX message_id_by_message_ptr;
-- ]

-- [ create-index-message_id_by_message_ptr
CREATE INDEX message_id_by_message_ptr ON message_id (message_ptr);
-- ]

-- [ drop-index-message_id_by_header
DROP INDEX message_id_by_header;
-- ]

-- [ create-index-message_id_by_header
CREATE INDEX message_id_by_header ON message_id (header);
-- ]
