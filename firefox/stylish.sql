BEGIN TRANSACTION;

DROP TABLE IF EXISTS style_meta;
CREATE TABLE style_meta (
        id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        style_id INTEGER NOT NULL,
        name TEXT NOT NULL,
        value TEXT NOT NULL);

INSERT INTO style_meta
        (style_id, name, value)
        VALUES(1, 'url-prefix', 'http://bikeguide.org/forums/showthread.php');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(1, 'url-prefix', 'http://www.bikeguide.org/forums/showthread.php');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(1, 'type', 'site');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(2, 'url-prefix', 'http://github.com/');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(2, 'type', 'site');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(3, 'url-prefix', 'http://www.discogs.com');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(3, 'type', 'site');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(4, 'url-prefix', 'http://bikesnobnyc.blogspot.com');
INSERT INTO style_meta
        (style_id, name, value)
        VALUES(4, 'type', 'site');

CREATE INDEX style_meta_style_id ON style_meta (style_id);

DROP TABLE IF EXISTS styles;
CREATE TABLE styles (
        id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        url TEXT,
        updateUrl TEXT,
        md5Url TEXT,
        name TEXT NOT NULL,
        code TEXT NOT NULL,
        enabled INTEGER NOT NULL,
        originalCode TEXT NULL);
INSERT INTO styles
        (name, enabled, code)
        VALUES('vBulletin Cleanup', 1,
'@-moz-document url-prefix(http://bikeguide.org/forums/showthread.php),
url-prefix(http://www.bikeguide.org/forums/showthread.php) {
  body {
    margin: 0 !important;
  }

  .bigusername {
    font-size: 10pt !important;
  }

  /* Remove header. */
  body > table:first-child {
    display: none !important;
  }

  /* Remove junk below each post. */
  table.tborder > tbody > tr + tr + tr {
    display: none;
  }

  /* Add border as we remove it when removing junk in previous rule. */
  table.tborder > tbody > tr + tr > td {
    border-bottom: 1px solid rgb(69, 92, 139) !important;
  }

  /* Deindent quostes. */
  div[style=''margin: 5px 20px 20px;''] {
    margin: 5px 0 10px 0 !important;
  }

  /* Remove “Quote:” text. */
  div[style=''margin: 5px 20px 20px;''] > div:first-child {
    display: none;
  }

  /* Add space between name of quotee and quoted text. */
  div[style=''margin: 5px 20px 20px;''] > table > tbody > tr > td > div:first-child {
    margin-bottom: 5px !important;
  }

  div.page > div > table > tbody > tr > td:first-child {
    display: none;
  }

  /* Remove previous/next thread links. */
  div.page > div > table + br + div {
    display: none;
  }

  /* Remove comment about becoming a premium member. */
  div#pagenav_menu + div > div > div {
    display: none;
  }
}');
INSERT INTO styles
        (name, enabled, code)
        VALUES('Github fixed-width font fixup', 1,
'@-moz-document url-prefix(http://github.com/) {
  #commit .human .message pre,
  #commit .machine,
  #commit .commit_oneline .commit,
  #commit .commit_oneline .tree,
  #forkqueue table td.sha,
  #forkqueue table td.message,
  #forkqueue table td.human,
  #toc,
  #browser table,
  #files textarea,
  #files .file,
  #repos .repo .commit .machine,
  .news pre,
  .news code,
  #code_search_instructions table.instruction tr td.inst,
  #code_search_results .result .snippet,
  #forkqueue table.choice td.code,
  #readme div.plain pre,
  #files .file .data pre,
  #files .file .line-data,
  #files .meta .bubble {
    font-family: monospace !important;
  }
}');
INSERT INTO styles
        (name, enabled, code)
        VALUES('Discogs cleanup', 1,
'@-moz-document url-prefix(http://www.discogs.com/) {
  body {
    background: white !important;
    font: 10pt sans-serif !important;
  }

  #page {
    border: none !important;
    background: white !important;
    padding: 0 !important;
  }

  #header {
    border-bottom: none !important;
  }

  #search_submit {
    width: 5em !important;
  }

/*  div.left {
    width: auto !important;
  }*/

  div.body div.image {
    float: right !important;
    margin-right: 20px !important;
    margin-left: 5px !important;
  }

  div.body div.profile {
    padding-left: 0 !important;
  }

  div.body div.profile h1 {
    font-size: 2em !important;
  }

  div.body div.profile div.head {
    color: inherit !important;
  }

  div.section h3 {
    color: inherit !important;
    font-size: 1.17em !important;
    border-bottom: none !important;
    padding: 0 0 1px !important
  }

  div.section.major h3 {
    color: inherit !important;
    background: none !important;
    padding: 4px 0 2px !important;
  }

  div.section div.section_content {
    padding: 5px 0 !important;
  }

  div.section.tracklist table {
    width: auto !important;
  }

  div.section.tracklist td {
    border-top: none !important;
  }

  div.section.tracklist td.track_duration {
    padding-left: 1em !important;
  }

  div.section.tracklist tr.track_extra_artists td {
    font-size: smaller !important;
  }

  div.section.credits span.role {
    color: inherit !important;
  }

  span.rating_value {
    font-size: 1em !important;
  }

  span.rating_value_sm {
    font-size: 1em !important;
  }

  div.review div.head {
    padding: 2px 0 !important;
  }

  div.review span.date {
    color: inherit !important;
    font-size: small !important;
  }

  div.review div.comment {
    margin: 5px 0 8px !important;
    width: 40em !important;
    text-align: justify !important;
  }

  div.review div.bottom {
    border-bottom: none !important;
  }

  div.review div.bottom span.tags {
    border-left: none !important;
  }

  div.review div.bottom span.tags .tag {
    color: #03b !important;
    border-top: none !important;
    border-right: none !important;
  }

  div.review div.bottom a.review_action1,
  div.review div.bottom a.review_action2 {
    font-size: small !important;
  }

  div.review div.bottom a.review_action2 {
    color: #03b !important;
  }

  table.releases td, table.releases th {
    border-bottom: none !important;
  }
}');

INSERT INTO styles
        (name, enabled, code)
        VALUES('Bikesnob Fixup', 1,
'@-moz-document url-prefix(http://bikesnobnyc.blogspot.com) {
  body, #outer-wrapper {
    font: 10pt sans-serif !important;
  }
  #outer-wrapper {
    text-align: justify !important;
  }
}');

COMMIT;
