// ==UserScript==
// @name          vBulletin Cleanup
// @namespace     http://bitwi.se/greasemonkey
// @description   Clean up vBulletin forums
// @include       http://bikeguide.org/forums/showthread.php?*
// @include       http://www.bikeguide.org/forums/showthread.php?*
// ==/UserScript==

function evaluateExpression(xpathExpression, contextNode, resultType) {
  return document.evaluate(xpathExpression,
                           contextNode ? contextNode : document,
                           null,
                           resultType ? resultType : XPathResult.ORDERED_NODE_SNAPSHOT_TYPE,
                           null);
}

function evaluateExpressionFirst(xpathExpression, contextNode) {
  return evaluateExpression(xpathExpression, contextNode, XPathResult.FIRST_ORDERED_NODE_TYPE).singleNodeValue;
}

function forEachNode(nodes, closure) {
  for (var i = 0; i < nodes.snapshotLength; i++)
    closure(nodes.snapshotItem(i));
}

function forEachNodeInExpression(xpathExpression, contextNode, closure) {
  forEachNode(evaluateExpression(xpathExpression, contextNode), closure);
}

var posts = evaluateExpression('//table[starts-with(@id, "post")]');

forEachNode(posts, function(post) {
  forEachNodeInExpression('tbody/tr[2]/td[1]/div[count(div)>=4]', post, function(node) {
    node.style.display = 'none';
  });

  var quoteLink = evaluateExpressionFirst('tbody/tr[3]/td[2]/a[1]', post);
  var newQuoteLink = document.createElement('a');
  newQuoteLink.href = quoteLink.href;
  newQuoteLink.textContent = 'Quote';
  var header = evaluateExpressionFirst('tbody/tr[1]/td[2]', post);
  header.insertBefore(newQuoteLink, header.firstChild);
});
