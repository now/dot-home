BEGIN TRANSACTION;
INSERT INTO moz_hosts (host, type, permission) VALUES('172.16.0.1', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('172.16.0.12', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('intranet.amesto.intra', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('intranett.amesto.no', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('amesto.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('sdl.com', 'cookie', 1);
COMMIT;
