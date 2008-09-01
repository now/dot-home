// ==UserScript==
// @name          Delicious Favicons
// @namespace     http://bitwi.se/greasemonkey
// @description   Display favicons associated with your Delicious bookmarks.
// @include       http://delicious.com/*
// ==/UserScript==

for (var links = document.evaluate('//li[contains(@class, "post")]//h4/a[1]', document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null),
     i = 0,
     link;
     link = links.snapshotItem(i);
     i++) {
  var favicon = new Image();
  favicon.src = 'http://google.com/s2/favicons?domain=' + link.hostname + '/favicon.ico';
  favicon.width = favicon.height = 16;
  favicon.setAttribute('style', 'float: left; margin-right: 1ex; border: 0;');
  link.parentNode.insertBefore(favicon, link);
}
