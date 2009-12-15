// ==UserScript==
// @name          Juno Records Download All Link
// @namespace     http://bitwi.se/greasemonkey
// @description   Adds a “download all” link to Juno record tracklisting pages
// @include       http://www.juno.co.uk/products/*
// ==/UserScript==

(function() {


var download = function() {
  for (var links = document.evaluate('//td[contains(@class, "tracklistmp3")]/a[2]', document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null),
      i = 0,
      link;
      link = links.snapshotItem(i);
      i++) {
    var win = window.open(link.href, 'download' + link.href + i);
    win.addEventListener('load', function(){ win.close(); }, true);
  }
  return false;
};

for (var containers = document.evaluate('//table[contains(@class, "productdatabuy")]/tbody[1]/tr[1]/td[1]', document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null),
     i = 0,
     container;
     container = containers.snapshotItem(i);
     i++) {
  var button = document.createElement('a');
  button.href = 'javascript:void(0);';
  button.textContent = 'Download all';
  button.addEventListener('click', download, true);
  container.appendChild(button);
}

})();
