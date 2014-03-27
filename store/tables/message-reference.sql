-- | message_reference @ application

-- [ drop-table-message_reference
DROP TABLE message_reference;
-- ]

-- [ create-table-message_reference
CREATE TABLE message_reference (
	id			INTEGER	UNSIGNED	PRIMARY KEY AUTO_INCREMENT,
	referencing_message_ptr	INTEGER	UNSIGNED	NOT NULL,                                   -- message that is referencing
	message_id_ptr		INTEGER	UNSIGNED	NOT NULL
) ENGINE = INNODB;
-- ]

-- [ drop-index-message_reference_by_referencing_message_ptr
DROP INDEX message_reference_by_referencing_message_ptr;
-- ]

-- [ create-index-message_reference_by_referencing_message_ptr
CREATE INDEX message_reference_by_referencing_message_ptr ON message_reference (referencing_message_ptr);
-- ]

-- [ drop-index-message_reference_by_message_id_ptr
DROP INDEX message_reference_by_message_id_ptr;
-- ]

-- [ create-index-message_reference_by_message_id_ptr
CREATE INDEX message_reference_by_message_id_ptr ON message_reference (message_id_ptr);
-- ]

