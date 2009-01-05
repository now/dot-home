(function () {

const oldGetSearchURL = liberator.bookmarks.getSearchURL;
const keywords = {
  mark: 'http://del.icio.us/post?v=4;url=%h;title=%t',
  del: 'http://del.icio.us/search?all=%s',
  "'": 'http://del.icio.us/now/%s',
  '#': 'http://hitta.se/default.aspx?UCSB%%3aTextBoxWho=%s&SearchType=4&UCSB%%3aWflWhite=1a1b&UCSB%%3aWflPink=4a',
  addr: 'http://hitta.se/SearchCombi.aspx?var=%s',
  movie: 'http://imdb.com/find?q=%s',
  posix: 'http://opengroup.org/cgi-bin/kman2?value=%s',
  wps: 'http://en.wikipedia.org/wiki/Special:Search?search=%s&fulltext=Search',
  wp: 'http://en.wikipedia.org/wiki/%Ws',
  '/': 'http://google.com/search?q=%s',
//  acronym: 'http://www.ucc.ie/cgi-bin/uncgi/acronym?%s',
  archived: 'http://web.archive.org/web/*/%h',
  cite: 'http://citeseer.ist.psu.edu/cis?q=%s&submit=Search+Documents',
  cookies: "javascript:alert('Cookies stored by this host or domain:\\n\\n' + document.cookie.replace(/; /g, '\\n'))",
  d: 'http://discogs.com/search?type=all&q=%s',
  '/d': 'http://google.com/search?q=%s+site:discogs.com',
  dict: 'http://wordnet.princeton.edu/perl/webwn?s=%s',
  login: 'http://bugmenot.com/view.php?mode=bookmarklet&url=%h',
  math: 'http://planetmath.org/?op=search&term=%s',
  q: 'http://answers.com/%s',
  related: 'http://google.com/search?related:%Lh',
  '/s': 'http://google.com/search?q=%s+site:%Lh',
  '/c': 'http://google.com/codesearch?q=%s',
  note: 'javascript:(function(){EN_CLIP_HOST="http://www.evernote.com";try{var%20x=document.createElement("SCRIPT");x.type="text/javascript";x.src=EN_CLIP_HOST+"/public/bookmarkClipper.js?"+(new%20Date().getTime()/100000);document.getElementsByTagName("head")[0].appendChild(x);}catch(e){location.href=EN_CLIP_HOST+"/clip.action?url="+encodeURIComponent(location.href)+"&title="+encodeURIComponent(document.title);}})();'
};

liberator.bookmarks.getSearchURL = function(text, useDefsearch) {
  text = (text == null ? "" : text);
  var url = keywords[text.replace(/^\s*(\S+).*/, '$1')];
  if (url) {
    text = text.replace(/^\s*\S+\s*/, "");
    var words = text.split(/\s+/)
    var expanded = [];
    var add_javascript = false;

    for (var i = 0; i < url.length; i++) {
      switch (url[i]) {
      case '%':
        var position = [];
        if (url[i + 1].match(/^\d$/)) {
          while (url[i + 1].match(/^\d$/))
            position.push(url[++i]);
          position = parseInt(position.join("")) - 1;
        } else {
          position = "";
        }

        var modifier = "";
        if (url[i + 1].match(/^[ULCW]$/))
          modifier = url[++i];

        var quote = false;
        switch (url[++i]) {
        case 's':
          var component = ""
          if (position === "")
            component = text;
          else
            component = (position < words.length ? words[position] : "");

          switch (modifier) {
          case 'U':
            component = component.toUpperCase();
            break;
          case 'L':
            component = component.toLowerCase();
            break;
          case 'C':
            component = component.split(/\s+/).map(function(s){ if (s.length == 0) return ""; return s[0].toUpperCase() + s.slice(1).toLowerCase(); }).join(' ');
            break;
          case 'W':
            component = component.split(/\s+/).map(function(s){ if (s.length == 0) return ""; return s[0].toUpperCase() + s.slice(1).toLowerCase(); }).join('_');
            break;
          }

          expanded.push(encodeURIComponent(component));
          break;
        case 'h':
          add_javascript = true;
          quote = true;
          if (modifier == 'L')
            expanded.push("'+encodeURIComponent(location.href.replace(/^(?:http:\\/\\/)?([^\\/]+).*/, '$1'))");
          else
            expanded.push("'+encodeURIComponent(location.href)");
          break;
        case 't':
          add_javascript = true;
          quote = true;
          expanded.push("'+encodeURIComponent(document.title)");
          break;
        case '%':
          expanded.push('%');
          break;
        default:
          expanded.push('%');
          expanded.push(position + 1);
          expanded.push(modifier);
          expanded.push(url[i]);
          break;
        }
        if (quote && i < url.length - 1)
          expanded.push("+'");
        break;
      default:
        expanded.push(url[i]);
        break;
      }
    }

    if (add_javascript)
      expanded.unshift("javascript:location.href='");

    expanded = expanded.join("");
    return expanded;
  }

  return oldGetSearchURL(text, keyword);
}

})();
