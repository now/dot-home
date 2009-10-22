// ==UserScript==
// @name          MediaFire Automatic Download
// @namespace     http://bitwi.se/greasemonkey
// @include       http://www.mediafire.com/download.php?*
// @include       http://www.mediafire.com/*
// ==/UserScript==

function shouldClick() {
  try {
    unsafeWindow.p_ct_link.href = unsafeWindow.p_ct_dl_url;
    unsafeWindow.p_ct_link.target = "";
    return true;
  } catch (e) {
    return false;
  }
}

window.addEventListener("load", function(e) {
  var link = document.getElementById('download_link').firstChild;
  if (!shouldClick()) {
    document.location = link.href;
    return;
  }
  try {
    var event = window.document.createEvent("MouseEvents");
    event.initMouseEvent("click", true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
    link.dispatchEvent(event);
  } catch (e){
  }
}, false);
