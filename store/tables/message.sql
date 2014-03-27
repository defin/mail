-- | message @ application

-- [ drop-table-message
DROP TABLE message;
-- ]

-- [ create-table-message
CREATE TABLE message (
	id			INTEGER UNSIGNED		PRIMARY KEY AUTO_INCREMENT,
	creation_time		INTEGER UNSIGNED,
	modification_time	INTEGER UNSIGNED,
	archive_message_ptr	INTEGER UNSIGNED,					-- id of archive_message object this message was parsed from
	message_id_ptr		INTEGER UNSIGNED,
	subject			VARCHAR(255),
	date			INTEGER UNSIGNED,					-- parsed from header_date
	from_ptr		INTEGER UNSIGNED,					-- email_address object
	to_ptr			INTEGER UNSIGNED,
	status			VARCHAR(255),
	flags			SET('unseen', 'answered', 'deleted', 'no reply needed', 'review later') NOT NULL,
	categorization		VARCHAR(255),
	is_reply_to_ptr		INTEGER UNSIGNED,
	outgoing_p		TINYINT 		NOT NULL DEFAULT 0,
	line_count		INTEGER	UNSIGNED	NOT NULL DEFAULT 0
) ENGINE = INNODB;
-- ]

-- [ drop-index-message_id_index
DROP INDEX message_id_index;
-- ]

-- [ create-index-message_id_index
CREATE INDEX message_id_index ON message (message_id_ptr);
-- ]

-- ALTER TABLE message ADD COLUMN line_count INTEGER UNSIGNED NOT NULL DEFAULT 0;
