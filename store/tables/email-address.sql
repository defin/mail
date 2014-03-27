-- | :db application :package mail-store

-- [
DROP TABLE email_address;
-- ]

-- [
CREATE TABLE email_address (
	id			INTEGER UNSIGNED	PRIMARY KEY AUTO_INCREMENT,
	creation_time		DATETIME		NOT NULL,
	modification_time	TIMESTAMP		NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	phrase			VARCHAR(255),
	local_part		VARCHAR(255),
	domain			VARCHAR(255),
	text			VARCHAR(255)
) ENGINE = INNODB;
-- ]

-- [
DROP INDEX email_addresses ON email_address;
-- ]

-- [
CREATE INDEX email_addresses ON email_address (text);
-- ]
