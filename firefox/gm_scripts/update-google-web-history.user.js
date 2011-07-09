// ==UserScript==
// @name          Update Google Web History
// @namespace     http://bitwi.se/greasemonkey
// @description   Update Google Web History without using Google Toolbar
// @include       *
// @exclude       http://www.google.tld/search?*
// @exclude       http://images.google.tld/images?*
// @exclude       http://maps.google.tld/maps?*
// @exclude       http://news.google.tld/news?*
// @exclude       http://www.google.tld/products?*
// @exclude       http://video.google.tld/*
// @exclude       http://books.google.tld/books?*
// @exclude       http://blogsearch.google.tld/blogsearch?*
// @exclude       http://www.google.tld/history/*
// @exclude       https://*
// ==/UserScript==

(function (url) {
  if (window.self != window.parent)
    return;

  if (/^https?:\/\/(?:www\.)?google\.com\/url\?/.test(document.referrer))
    return;

  function r(x, y) {
    return Math.floor((x / y - Math.floor(x / y)) * y + .1);
  };

  function ch(url) {
    var data = 'info:' + url;
    var c = [0x9e3779b9, 0x9e3779b9, 0xe6359a60];
    function m(c) {
      var s = [13, 8, 13, 12, 16, 5, 3, 10, 15];
      for (var i = 0; i < 9; i++) {
        var j = c[r(i + 2, 3)];
        c[r(i, 3)] = (c[r(i, 3)] - c[r(i + 1, 3)] - j) ^ (r(i, 3) == 1 ? j << s[i] : j >>> s[i]);
      }
    };

    var l, k = 0;
    for (l = data.length; l >= 12; l -= 12) {
      for (var i = 0; i < 16; i++)
        c[Math.floor(i / 4)] += data.charCodeAt(k + i) << r(k + i, 4) * 8;

      m(c);

      k += 12;
    }

    c[2] += data.length;

    for (var i = l; i > 0; i--)
      c[Math.floor((i - 1) / 4)] += data.charCodeAt(k + i - 1) << (r(i - 1, 4) + (i > 8 ? 1 : 0)) * 8;

    m(c);

    return '6' + c[2];
  };

  new Image().src = 'http://www.google.com/search?client=navclient-auto&ch=' +
    ch(url) + '&features=Rank&q=info:' + escape(url);
})(location);
