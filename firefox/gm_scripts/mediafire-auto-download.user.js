// ==UserScript==
// @name          MediaFire Automatic Download
// @namespace     http://bitwi.se/greasemonkey
// @description   Download uploaded content on MediaFire automatically.
// @include       http://www.mediafire.com/?*
// ==/UserScript==

window.addEventListener("DOMNodeInserted", function (e) {
  var link = e.target;
  if (!(link.getAttribute("href") &&
        /start download/.test(link.textContent) &&
        /block/.test(link.parentNode.getAttribute('style'))))
    return;
  document.body.innerHTML = '<a href="' + link + '">' + link + '</a>';
  location.href = link.href;
}, true);
