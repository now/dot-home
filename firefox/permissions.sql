BEGIN TRANSACTION;

DROP TABLE IF EXISTS moz_hosts;
CREATE TABLE moz_hosts (id INTEGER PRIMARY KEY,
                        host TEXT,
                        type TEXT,
                        permission INTEGER);

/* Default Entries */
INSERT INTO moz_hosts (host, type, permission) VALUES('addons.mozilla.org', 'install', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('update.mozilla.org', 'install', 1);

COMMIT;
