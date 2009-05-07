commands.addUserCommand(['deli[cious]'], 'Bookmark page at Delicious', function(args) {
  var target = buffer.URL;
  var description = args['-description'] || buffer.title;
  var note = args['-note'];
  var shared = args['-private'] ? 'no' : null;
  var url = ['https://api.del.icio.us/v1/posts/add?'];
  [['url', target],
   ['description', description],
   ['extended', note],
   ['tags', args[0]],
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
  argCount: '1',
  options: [[['-description', '-d'], commands.OPTION_STRING, null, function() [[buffer.title]]],
            [['-note', '-n'], commands.OPTION_STRING, null, null],
            [['-private', '-p'], commands.OPTION_NOARG]]
}, true);
