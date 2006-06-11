// ==UserScript==
// @name          Secure GMail Connector
// @namespace     http://bitwi.se/greasemonkey
// @description   When connecting to GMail, makes sure that HTTPS is used.
// @include       http://mail.google.com/*
// ==/UserScript==
// 
// Based on a script by Mark Pilgrim.

location.href = location.href.replace(/^http:/, 'https:');
