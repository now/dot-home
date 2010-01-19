BEGIN TRANSACTION;
INSERT INTO moz_hosts (host, type, permission) VALUES('intranett.amesto.no', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('amesto.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('sdl.com', 'cookie', 1);
COMMIT;
