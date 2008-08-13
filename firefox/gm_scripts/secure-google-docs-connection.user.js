// ==UserScript==
// @name          Secure Google Docs Connection
// @namespace     http://bitwi.se/greasemonkey
// @description   Make sure that HTTPS is used when connecting to Google Docs.
// @include       http://docs.google.com/*
// ==/UserScript==

location.href = location.href.replace(/^http:/, 'https:');
