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
    font: 11pt sans-serif !important;
    margin: 0 !important;
  }

  a:link, body_alink,
  .tcat a:link, .tcat_alink,
  .thead a:link .thead_alink,
  .tfoot a:link, .tfoot_alink,
  .vbmenu_control a:link, .vbmenu_control_alink,
  .vbmenu_option a:link, .vbmenu_option_alink,
  .vbmenu_hilite a:link, .vbmenu_hilite_alink {
    color: #2f5a9b !important;
  }

  a:visited, body_avisited,
  .tcat a:visited, .tcat_avisited,
  .thead a:visited, .thead_avisited,
  .tfoot a:visited, .tfoot_avisited,
  .vbmenu_control a:visited, .vbmenu_control_avisited,
  .vbmenu_option a:visited, .vbmenu_option_avisited,
  .vbmenu_hilite a:visited, .vbmenu_hilite_avisited {
    color: #602f80 !important;
  }

  a:hover, a:active, body_ahover,
  .tcat a:hover, .tcat a:active, .tcat_ahover,
  .thead a:hover, .thead a:active, .thead_ahover,
  .tfoot a:hover, .tfoot a:active .tfoot_avisited,
  .vbmenu_control a:hover, .vbmenu_control a:active, .vbmenu_control_ahover,
  .vbmenu_option a:hover, .vbmenu_option a:active, .vbmenu_option_ahover,
  .vbmenu_hilite a:hover, .vbmenu_hilite a:active, .vbmenu_hilite_ahover {
    color: #f02626 !important;
  }

  .page {
    background: #f6f6f6 !important;
    color: #181818 !important;
  }

  .page a:link, .page_alink {
    background: none !important;
  }

  td, th, p, li, .wysiwyg, textarea, .bginput, .button, select, legend,
  .vbmenu_option, .bigusername {
    font: inherit !important;
  }

  td {
    border: none !important;
  }

  .tborder, .panel, .vBulletin_editor {
    background: none !important;
    color: inherit !important;
    border: none !important;
    margin-bottom: 1em !important;
  }

  .tcat, .thead, .vbmenu_control {
    background: none !important;
    color: inherit !important;
    font: inherit !important;
  }

  .thead {
    font-size: smaller !important;
  }

  .alt1, .alt1Active, .alt2, .alt2Active {
    background: inherit !important;
    color: inherit !important;
  }

  .alt1 > div.smallfont {
    font-size: 100% !important;
    margin-bottom: 1em !important;
  }

  .bginput option, .bginput optgroup, option, optgroup {
    font-size: 100% !important;
    font-family: sans-serif !important;
  }

  fieldset {
    border: none !important;
  }

  legend {
    color: inherit !important;
    font-weight: bold !important;
  }

  .smallfont, .navbar {
    font-size: smaller !important;
  }

  .time, .highlight {
    color: inherit !important;
  }

  .vbmenu_popup, .panelsurround {
    background: inherit !important;
    color: inherit !important;
  }

  .fieldset, .fieldset td, .fieldset p, .fieldset li {
    font-size: 100% !important;
  }

  /* Remove header,
     horizontal rule below thread subject */
  body > table:first-child, hr,
  body > div > div > div > center + table {
    display: none !important;
  }

  /* Remove junk below each post. */
  table.tborder > tbody > tr + tr + tr {
    display: none;
  }

  /* Deindent quotes. */
  div[style=''margin: 5px 20px 20px;''] {
    margin: 0 0 1em 3em !important;
  }

  /* Remove “Quote:” text. */
  div[style=''margin: 5px 20px 20px;''] > div:first-child {
    display: none;
  }

  /* Add space between name of quotee and quoted text. */
  div[style=''margin: 5px 20px 20px;''] > table > tbody > tr > td > div:first-child {
    margin-bottom: 5px !important;
  }

  /* Remove super-ugly buttons. */
  div.page > div > table > tbody > tr > td:first-child {
    display: none;
  }

  form[style=''clear: left;''] {
    display: none !important;
  }

  /* Remove stupid breaks,
     previous/next thread links,
     forum jump links,
     advertisements,
     sitemeter */
  body > div > div > div > br, 
  body > div > div > div > br + div,
  body > div > div > div > br + table,
  body > div > div > div > div + table,
  body > div > div > div > table + br + center,
  body > form + script + div {
    display: none;
  }

  /* Remove comment about becoming a premium member. */
  div#pagenav_menu + div > div > div {
    display: none;
  }

  /* Remove uninteresting information about length of membership. */
  td.alt2[width="175"] > div.smallfont {
    display: none;
  }
}');
INSERT INTO styles
        (name, enabled, code)
        VALUES('Github fixed-width font fixup', 1,
'@-moz-document url-prefix(https://github.com/) {
  #readme div.plain pre,
  .blob-editor textarea,
  .add-pill-form textarea.key_value,
  dl.keyboard-mappings dt,
  #code_search_instructions table.instruction tr td.inst,
  #code_search_results .result .snippet,
  .commit-ref,
  .commits-condensed code,
  .commits-condensed tr.merge td.author,
  .commits-condensed tr.merge td.date,
  .commit-preview .message p.commit-id,
  #forkqueue table td.sha,
  #forkqueue table td.message,
  #forkqueue table td.human,
  #forkqueue table.choice td.code,
  #repos .repo .commit .machine,
  input.url-field,
  #commit .human message,
  #commit .machine,
  #commit .commit_oneline .commit,
  #commit .commit_oneline .tree,
  #toc,
  #browser table,
  #files .file .meta .info,
  #files .file .data pre,
  #files .file .line-data,
  #files .file .line-number,
  #files .file .data td.line_numbers,
  #files .meta .bubble,
  .news pre,
  .news code,
  #history table.commits td.sha,
  #history table.commits td.message,
  #history table.commits td.human {
    font-family: monospace !important;
  }
  #files .file .data,
  #files .file .data pre,
  #files .file .data .line-data,
  #files .file .data .line-number
  #files .file .data td.line_numbers {
    font-size: 100% !important;
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
  .post-body div {
    text-align: justify !important;
  }
}');

COMMIT;
