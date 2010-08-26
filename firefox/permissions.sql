BEGIN TRANSACTION;
DROP TABLE IF EXISTS moz_hosts;
CREATE TABLE moz_hosts (id INTEGER PRIMARY KEY, host TEXT, type TEXT, permission INTEGER);
/* Default Entries */
INSERT INTO moz_hosts (host, type, permission) VALUES('addons.mozilla.org', 'install', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('update.mozilla.org', 'install', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('tools.google.com', 'install', 1);

/* Stores */
INSERT INTO moz_hosts (host, type, permission) VALUES('adlibris.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('store.apple.come', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('bolagret.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('caliroots.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('camper.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('chainreactioncycles.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('chemical-records.co.uk', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('childstore.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('complyfoam.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('cyclecomponents.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('danscomp.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('decks.co.uk', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('ebay.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('ebay.co.uk', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('flerbollar.e-butik.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('hollywood.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('jula.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('junkyard.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('juno.co.uk', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('juno.co.uk', 'popup', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('komplett.se', 'popup', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('on-one-shop.co.uk', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('pragprog.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('safaribooksonline.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('shelta.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('sneakersnstuff.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('svalander.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('store.apple.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('ultimateears.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('yoyoguy.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('yoyonation.com', 'cookie', 1);

/* Merchant Payment Interfaces */
INSERT INTO moz_hosts (host, type, permission) VALUES('arcot.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('auriganet.eu', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('secure.incab.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('paypal.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('pay-read.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('posten.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('publishme.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('epostboxen.posten.se', 'cookie', 8);

/* Utilities */
INSERT INTO moz_hosts (host, type, permission) VALUES('blogger.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('blocket.se', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('csn.se', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('delicious.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('discogs.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('dreamhost.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('dropbox.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('dyndns.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('evernote.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('facebook.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('github.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('gotapi.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('gotlib.goteborg.se', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('hemnet.se', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('hypem.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('iis.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('myopenid.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('nonoh.net', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('nytimes.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('posterous.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('reddit.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('residentadvisor.net', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('sf.se', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('soundcloud.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('sugarsync.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('telia.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('tvrage.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('tumblr.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('vasttrafik.se', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('vimeo.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('voddler.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('wikipedia.org', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('yahoo.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('youtube.com', 'cookie', 1);

/* Forums */
INSERT INTO moz_hosts (host, type, permission) VALUES('bikeguide.org', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('ghisler.ch', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('rubyforge.org', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('skatteverket.se', 'cookie', 8);

/* Storage Sites */
INSERT INTO moz_hosts (host, type, permission) VALUES('divshare.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('mediafire.com', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('zshare.net', 'cookie', 8);

/* Banking */
INSERT INTO moz_hosts (host, type, permission) VALUES('nb.se', 'cookie', 8);
INSERT INTO moz_hosts (host, type, permission) VALUES('nordea.se', 'cookie', 8);

/* Real Estate Agents */
INSERT INTO moz_hosts (host, type, permission) VALUES('capitex.se', 'cookie', 8);

/* Miscellaneous */
INSERT INTO moz_hosts (host, type, permission) VALUES('google.com', 'cookie', 1);
INSERT INTO moz_hosts (host, type, permission) VALUES('google.se', 'cookie', 1);

COMMIT;
