-- | mailbox @ application

-- [ drop-table-mailbox
DROP TABLE mailbox;
-- ]

-- [ create-table-mailbox
CREATE TABLE mailbox (
	id			INTEGER	UNSIGNED	PRIMARY KEY AUTO_INCREMENT,
	creation_time		INTEGER UNSIGNED,
	modification_time	INTEGER UNSIGNED, 
	name			VARCHAR(255),
	description		TEXT,							-- english description of mailbox
	automatically_generated	TINYINT
) ENGINE = INNODB;
-- ]

-- ALTER TABLE mailbox DELETE COLUMN temporary_p;
