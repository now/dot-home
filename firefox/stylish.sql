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

COMMIT;
