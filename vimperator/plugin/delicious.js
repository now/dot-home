commands.addUserCommand(['deli[cious]'], 'Bookmark page at Delicious', function(args) {
  var target = buffer.URL;
  var description = args['-description'] || buffer.title || buffer.URL;
  var note = args['-note'];
  var shared = args['-private'] ? 'no' : null;
  var url = ['https://api.del.icio.us/v1/posts/add?'];
  [['url', target],
   ['description', description],
   ['extended', note],
   ['tags', args.join(" ")],
   ['shared', shared]].forEach(function(query) {
    if (query[1])
      url.push('&', query[0], '=', encodeURIComponent(query[1]));
  });
  var xhr = new XMLHttpRequest();
  xhr.open('POST', url.join(""));
  xhr.onreadystatechange = function(event) {
    if (xhr.readyState != 4)
      return;

    if (xhr.status != 200) {
      liberator.echo('Bookmarking ' + target + ' at Delicious failed with status ' + xhr.status);
      return;
    }

    liberator.echo('Bookmarking ' + target + ' at Delicious ' +
                   xhr.responseXML.documentElement.getAttribute('code'));
  };
  xhr.send(null);
}, {
  argCount: '*',
  options: [[['-description', '-d'], commands.OPTION_STRING, null, function() [[buffer.title]]],
            [['-note', '-n'], commands.OPTION_STRING, null, null],
            [['-private', '-p'], commands.OPTION_NOARG]],
  completer: function(context) {
    if (context.result) {
      context.completions = context.result;
      return;
    }

    context.title = ['Tags', 'Type'];
    context.incomplete = true;

    var url = ['https://api.del.icio.us/v1/posts/suggest?'];
    [['url', buffer.URL]].forEach(function(query) {
      if (query[1])
        url.push('&', query[0], '=', encodeURIComponent(query[1]));
    });

    var xhr = util.httpGet(url.join(""), function(xhr) {
      context.incomplete = false;
      var result = [];

      if (xhr.status != 200) {
        context.completions = [];
        return;
      }

      var tags = xhr.responseXML.documentElement.getElementsByTagName('*');
      for (var i = 0; i < tags.length; i++)
        result.push([tags[i].textContent, tags[i].localName]);

      context.completions = context.result = result;
    });
    context.cancel = function() xhr.abort();
  }
}, true);
