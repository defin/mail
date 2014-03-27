-- | canned_message @ application

-- [ drop-table-canned_message
DROP TABLE canned_message;
-- ]

-- [ create-table-canned_message
CREATE TABLE canned_message (
	id			INTEGER UNSIGNED		PRIMARY KEY AUTO_INCREMENT,
	creation_time		INTEGER UNSIGNED,
	modification_time	INTEGER UNSIGNED,
	name			VARCHAR(255)			NOT NULL,
	body			TEXT				NOT NULL
) ENGINE = INNODB;
-- ]

-- [ create-index-canned_message_by_name
CREATE INDEX canned_message_by_name ON canned_message (name);
-- ]
