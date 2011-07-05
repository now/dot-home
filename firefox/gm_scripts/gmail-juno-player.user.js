// ==UserScript==
// @name          GMail Juno Player
// @namespace     http://bitwi.se/greasemonkey
// @description   Display Juno Player in GMail for e-mails containing juno links
// @include       https://mail.google.com/*
// @require       http://ajax.googleapis.com/ajax/libs/jquery/1.6.0/jquery.min.js
// ==/UserScript==

unsafeWindow.gmonkey.load('1.0', function (gmail) {
  gmail.registerViewChangeCallback(function () {
    if (gmail.getActiveViewType() != 'cv')
      return;

    var root = gmail.getActiveViewElement();

    if ($('span[email="subscriptions@lists.juno.co.uk"]', root).length < 1)
      return;

    $('a[href^="http://www.juno.co.uk/products/"]', root).each(function () {
      $(this).after(
        '<p> \
          <embed \
            width="400" \
            height="130" \
            type="application/x-shockwave-flash" \
            flashvars="product_key=' + $(this).attr('href').replace(/^.*\/(.+)\.htm$/, '$1') + '" \
            src="http://www.junostatic.com/ultraplayer/06/MicroPlayer.swf" /> \
        </p>'
      );
    });
  });
});
