-- | draft_message @ application

-- [ drop-table-draft_message
DROP TABLE draft_message;
-- ]

-- [ create-table-draft_message
CREATE TABLE draft_message (
	id			INTEGER UNSIGNED		PRIMARY KEY AUTO_INCREMENT,
	creation_time		INTEGER UNSIGNED,
	modification_time	INTEGER UNSIGNED,
	headers			TEXT				NOT NULL,
	body			TEXT				NOT NULL
) ENGINE = INNODB;
-- ]
