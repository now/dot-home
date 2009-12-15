// ==UserScript==
// @name          Google Reader View Original in Background
// @namespace     http://bitwi.se/greasemonkey
// @description   View original command for Google Reader that opens target in the background
// @include       http://www.google.com/reader/view/*
// ==/UserScript==

document.addEventListener('keypress', function(event) {
  if (event.which != 118)
    return;

  var link = document.evaluate('(id("current-entry")//a)[1]', document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null).snapshotItem(0);
  if (!link)
    return;

  event.stopPropagation();
  event.preventDefault();

  GM_openInTab(link.href);
}, true);
