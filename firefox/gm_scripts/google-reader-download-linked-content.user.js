// ==UserScript==
// @name          Google Reader Download Linked Content
// @namespace     http://bitwi.se/greasemonkey
// @description   Download linked content in blog posts in Google Reader
// @include       http://www.google.com/reader/view/*
// ==/UserScript==

document.addEventListener('keypress', function(event) {
  if (event.which != 100)
    return;

  var links = document.evaluate('id("current-entry")//a',
                               document,
                               null,
                               XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,
                               null)
  if (!links)
    return;

  /* Open certain blog entries in tabs for download */
  var post = links.snapshotItem(0);
  if (!post)
    return;

  if (/^https?:\/\/(?:djsarepersons|beatelectric|soundsofthe70s|thenumber20|zyron|feedproxy\.google\.com\/~r\/(?:DiscoDelicious|FeelMyBicep)\/)/.test(post.href)) {
    GM_openInTab(post.href);
    return;
  }

  event.stopPropagation();
  event.preventDefault();

  var downloaded = [];
  // /^https?:\/\/(?:(?:www\.)?divshare\.com\/direct|mp3\.juno\.co\.uk\/MP3)\//
  for (var i = 0, link; link = links.snapshotItem(i); i++) {
    var found = false;
    for (var j = 0; j < downloaded.length; j++)
      if (link.href == downloaded[j]) {
        found = true;
        break;
      }
    if (found)
      continue;
    if (/\.mp3$/.test(link.href)) {
      GM_openInTab(link.href);
      downloaded.push(link.href);
      /*
      location.href = link.href;
      window.setTimeout(next, 500);
      */
    } else if (/^https?:\/\/(?:www\.)?(?:(?:divshare|mediafire|soundcloud)\.com|(?:zshare)\.net)\//.test(link.href)) {
      GM_openInTab(link.href);
      downloaded.push(link.href);
    }
  }
}, true);
