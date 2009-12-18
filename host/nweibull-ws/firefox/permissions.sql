BEGIN TRANSACTION;
INSERT INTO moz_hosts (host, type, permission) VALUES('sdl.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('intranett.amesto.no', 'cookie', 8);
COMMIT;
