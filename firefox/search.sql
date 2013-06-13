BEGIN TRANSACTION;
DROP TABLE IF EXISTS engine_data;
CREATE TABLE engine_data (id INTEGER PRIMARY KEY, engineid STRING, name STRING, value STRING);
/* Aliases */
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/google.xml', 'alias', 'google');
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/wikipedia.xml', 'alias', 'wikipedia');

INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/adlibris.xml', 'alias', 'adlibris');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/codesearch.xml', 'alias', 'codesearch');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/discogs.xml', 'alias', 'discogs');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/hittase-where.xml', 'alias', 'where');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/hittase-who.xml', 'alias', 'who');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/imdb.xml', 'alias', 'imdb');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/juno-records.xml', 'alias', 'juno');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/mancx.xml', 'alias', 'man');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/gatherer.xml', 'alias', 'mtg');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/posix.xml', 'alias', 'posix');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/thepiratebayse.xml', 'alias', 'tpb');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/tvragecom.xml', 'alias', 'tvrage');
INSERT INTO engine_data (engineid, name, value) VALUES('[profile]/youtube.xml', 'alias', 'youtube');

/* Hidden */
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/amazondotcom.xml', 'hidden', '1');
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/answers.xml', 'hidden', '1');
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/bing.xml', 'hidden', '1');
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/creativecommons.xml', 'hidden', '1');
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/eBay.xml', 'hidden', '1');
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/twitter.xml', 'hidden', '1');
INSERT INTO engine_data (engineid, name, value) VALUES('[app]/yahoo.xml', 'hidden', '1');
COMMIT;
