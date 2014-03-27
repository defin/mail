-- | deliverable_message @ application

-- [ drop-table-deliverable_message
DROP TABLE deliverable_message;
-- ]

-- [ create-table-deliverable_message
CREATE TABLE deliverable_message (
	id			INTEGER UNSIGNED		PRIMARY KEY AUTO_INCREMENT,
	creation_time		INTEGER UNSIGNED,
	modification_time	INTEGER UNSIGNED,		-- doubles as last retry time
	message_ptr		INTEGER UNSIGNED,
	host			VARCHAR(255),			-- host to connect to to deliver this message
	protocol		ENUM('SMTP', 'HTTP'),
	envelope_from		TEXT,
	envelope_to		TEXT,
	data			LONGTEXT,
	status			ENUM('queued', 'attempting', 'retry', 'failed', 'delivered') NOT NULL,
	retry_count		INTEGER UNSIGNED		NOT NULL DEFAULT 0
) ENGINE = INNODB;
-- ]
