// ==UserScript==
// @name          Divshare Automatic Download
// @namespace     http://bitwi.se/greasemonkey
// @description   Download uploaded content on Divshare automatically.
// @include       http://*.divshare.com/*
// ==/UserScript==

var links = document.links;
for (var i = 0; i < links.length; i++) {
  var link = links[i];
  if (/\.divshare\.com\/launch\.php\?/.exec(link.href)) {
    location.href = link.href;
    break;
  }
}
