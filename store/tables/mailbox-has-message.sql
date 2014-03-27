-- | mailbox_has_message @ application

-- [ drop-table-mailbox_has_message
DROP TABLE mailbox_has_message;
-- ]

-- [ create-table-mailbox_has_message
CREATE TABLE mailbox_has_message (
	id			INTEGER UNSIGNED	PRIMARY KEY AUTO_INCREMENT,
	creation_time		INTEGER	UNSIGNED	NOT NULL,
	modification_time	INTEGER	UNSIGNED	NOT NULL,
	mailbox_ptr		INTEGER	UNSIGNED	NOT NULL,
	message_ptr		INTEGER	UNSIGNED	NOT NULL,
	marked_deleted		TINYINT 		NOT NULL DEFAULT 0
) ENGINE = INNODB;
-- ]

-- [ drop-index-mailbox
DROP INDEX mailbox;
-- ]

-- [ create-index-mailbox
CREATE INDEX mailbox ON mailbox_has_message (mailbox_ptr);
-- ]

-- [ drop-index-message
DROP INDEX message;
-- ]

-- [ create-index-message
CREATE INDEX message ON mailbox_has_message (message_ptr);
-- ]

