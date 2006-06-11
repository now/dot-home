// ==UserScript==
// @name          Gmail Fixed Font Toggle
// @namespace     http://bitwi.se/greasemonkey
// @description	  Adds a fixed font size toggle button.
// @include       http://mail.google.com/*
// @include       https://mail.google.com/*
// ==/UserScript==
//
// This script is based on the script written by mihai@persistent.info.

(function() {

const CSS_RULE_MONOSPACE = '.mb, textarea.tb { font-family: monospace !important; }';
const CSS_RULE_NORMAL = '.mb, textarea.tb { }';

const TOGGLE_FONT_IMAGE = 'data:image/gif;base64,' +
              'R0lGODlhEAAQAIABAAAAzP%2F%2F%2FyH5BAEAAAEALAAAAAAQABAAAAImjI%' +
              '2BJwO28wIGG1rjUlFrZvoHJVz0SGXBqymXphU5Y17Kg%2BnixKBYAOw%3D%3D';

const LINKS_CONTAINER_ID = 'ap';
const LINKS_CONTAINER_LINK_CLASSNAME = 'ar';

var styleSheet = { };
var toggleFontLink = null;

function getLinksContainer() {
  return document.getElementById(LINKS_CONTAINER_ID);
}

function initializeToggleFont() {
  var linksContainer = getLinksContainer();
  if (!linksContainer)
    return;

  toggleFontLink = document.createElement('div');
  toggleFontLink.className = LINKS_CONTAINER_LINK_CLASSNAME;
  toggleFontLink.addEventListener('click', toggleMessageBodyFont, false);
  toggleFontLink.innerHTML =
    '<span class="l">' +
      '<img class="ai" width="16" height="16" src="' + TOGGLE_FONT_IMAGE + '">' +
      '<u>Toggle font</u>' +
    '</span>';

  linksContainer.appendChild(toggleFontLink);
  checkToggleFontParent();
}

function checkToggleFontParent() {
  var linksContainer = getLinksContainer();
  if (!linksContainer)
    return;

  if (toggleFontLink.parentNode != linksContainer)
    linksContainer.appendChild(toggleFontLink);
  
  window.setTimeout(checkToggleFontParent, 200);
}

function toggleMessageBodyFont() {
  styleSheet.currentRule = (styleSheet.currentRule == CSS_RULE_NORMAL) ? 
                           CSS_RULE_MONOSPACE :
                           CSS_RULE_NORMAL;
  styleSheet.node.deleteRule(0);
  styleSheet.node.insertRule(styleSheet.currentRule, 0);
}

function initializeStyleSheet() {
  var styleNode = document.createElement('style');
  document.body.appendChild(styleNode);
  styleSheet.node = document.styleSheets[document.styleSheets.length - 1];
  styleSheet.currentRule = CSS_RULE_NORMAL;
  styleSheet.node.insertRule(styleSheet.currentRule, 0);    
}

initializeStyleSheet();
initializeToggleFont();

})();
