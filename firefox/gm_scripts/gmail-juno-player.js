// ==UserScript==
// @name          GMail Juno Player
// @namespace     http://bitwi.se/greasemonkey
// @description   Display Juno Player in GMail for e-mails containing juno links
// @include       https://mail.google.com/*
// @require       http://code.jquery.com/jquery-latest.min.js
// ==/UserScript==

unsafeWindow.gmonkey.load('1.0', function (gmail) {
  gmail.registerViewChangeCallback(function () {
    if (gmail.getActiveViewType() != 'cv')
      return;

    window.setTimeout(function () {
      var div = gmail.getActiveViewElement();
      $('a[href^="http://www.juno.co.uk/products/"]', div).each(function () {
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
    }, 600);
  });
});
