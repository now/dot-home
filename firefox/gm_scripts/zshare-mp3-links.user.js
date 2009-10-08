// ==UserScript==
// @name          zSHARE MP3 Direct Download
// @namespace     http://bitwi.se/greasemonkey
// @description   Add a direct-download link to zSHARE pages for MP3s.
// @include       http://www.zshare.net/audio/*
// ==/UserScript==

var player = document.getElementById('Player');
if (player == null)
  return;

var url = player.getElementsByTagName('param').namedItem('URL');
if (url == null)
  return;

var link = document.createElement('a');
link.href = url.value;
link.textContent = document.getElementsByTagName('title')[0].textContent.replace(/^\s*zSHARE\s*-\s*/, "");

var title = document.createElement('p');
title.appendChild(link);

while (document.body.firstChild)
  document.body.removeChild(document.body.firstChild);

for (var i = 0; i < document.styleSheets.length; i++)
  document.styleSheets[i].disabled = true;

document.body.appendChild(title);

location.href = link.href;
