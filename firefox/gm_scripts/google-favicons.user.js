// ==UserScript==
// @name            Google Favicons
// @namespace       http://bitwi.se/
// @description     Shows site’s favicons for google searches
// @include         http://*.google.*/search?*
// ==/UserScript==

var generic_icon = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH1QQWFBMzy8WilwAAAlJJREFUOMuVkr1PU1EYxn/n9tIrUGx6gUIhQqx8SIiAGBwcjERdMCEmDkZl0U3j6KIxIV38BwwmGKODHwMxLurg5GBcQCYhpi0BTa0U6O2HLfe2t+29DqbYK8aEZ3pP8p7n/N7zvGJ2dvZlIpG4wN50cXp6eg6AUChk71WhUMiuOsnVQtM0LMsCQAix81S1FkLgcrnwer0OlB0Dy7KIRCKOC7USQjAwMLBrFrn20N/f/1+Cf0mubawlqOrzWoalr1myhkXZsnHLElKpnct3nl19cW/qiYOgr69vx+D75k8evlog0KYyeeYYBzubaVAU0nmDldgWi8uxx5duP73uMIhGowAkUgavFzY4Pz7McK+fdN5gNfoF2eOHeh9N++DKxAjzy/ExxwhVgrlH75k8NcSRoEoqmaClaxBvSwC9UCIRW0W2TKSKwemxoPMTI5EIn8IaPt9+xsd6yGU0yrIHJEGdJOOus2lQZHwNCgF/K4ri3p3C28WPnBjtIpNOkjQb8Xd0YNtgAwgJYZmspywWV5bw1dvOFMLhMAlNp7vdx3x0k2CvH8uGai75TIrjI4MIIbBtm/j6BtLfBAWzjKK4kTx+MvkCWzkTvVghp5uY+S3HXrS1Nv8hUFX192LIEtntAo2FH3hkhc3tFpJ1bsoVi0KqzEipRLFo0uRpRNd1xMzMzJtkMnmuahS3DnH25DBHe1SCBwJ8i8X5sGJg4sYolslmUiiiwo3JITRNY9d+Xrv7/H57QL05NTHK4e5WANKZLA/erVHBxXbBxDBMbk10ksvl+AWD+hkUofZ2KAAAAABJRU5ErkJggg%3D%3D';

var malware_icon = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH1gIQDictt+6SdwAAAehJREFUOMuVk8FKG2EUhb87mc40iTODEJlK7UZDxCCGQUqaMASCSGODdBe6ECJddOcwG/EJXBt0ZcBCwEVJVgWtr1AIPoBQXAQKhRZc1EBJMX831hLItPbAvYvL4Ttnc4UIhfDIgLcAA3jdgC/jfHoUwISjeXgOcAFHQHWcT4tI91KwWlhY0J6kUloKVkPw7g0wYS8NemJ3l0yzSRp0E/buBQihOAXFbLksej6Pns+TLZdlCoohFPmXdqB7AsNep6M8z1Oe56lep6NOYLgD3b82CGHFhaWM74vh+ziOg+M4GL5PxvfFhaUQViIBJuxnwUgEAaJpWJaFZVmIppEIArJgmLA/FhDC+jSk5woFjFIJEcG2bWzbRkQwSiXmCgWmIR3C+gggBDGgsXibruk6IvKngQiarpMIAhbBMKARgsDtCuHVLLReLC8bk2dnEIuhlKLf7wOQTCYREbi54WptjQ/n54NLqDfgnYQQewxXT8GaPz7mYbWKUgqlFLVaDYB2u42IICL8OD3lYmODLnz/DJOawGYczJlcjnilcmcUEVzXxXXdkVu8UmEmlyMOpsCmbMPXl5CaPThgol6/S/89wAhARLhutbjc2uI9fJNDGD4DecD/6SfwEZT+CQ4H8GYi4i+idA3DHjR/AZfefQgctOETAAAAAElFTkSuQmCC';

var links = document.evaluate('//*[@class="g w0"]//a[@class="l"]', document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
for (var i = 0; i < links.snapshotLength; i++) {
  var link = links.snapshotItem(i);
  var url = link.href.replace(/<\S[^><]*>/g, "");

  var img = document.createElement('img');
  img.width = img.height = 16;
  img.setAttribute('style', 'border: 0; padding-top: 2px; padding-right: 4px; display: block; float: left');
  if (url.match("google.com/interstitial?")) {
    img.src = malware_icon;
  } else if (url.indexOf('https://') < 0) {
    img.src = generic_icon;
    // We need a new scope for the event listener.
    // I wonder what a Vulcan would think of this.
    // Using let would be prettier, but GreaseMonkey isn't 1.7 compatible.
    (function(img, url){
      var favicon = new Image();
      favicon.addEventListener('load', function(){ img.src = url }, true);
      favicon.src = url;
    })(img, 'http://google.com/s2/favicons?domain=' + link.hostname);
  } else {
    img.src = generic_icon;
  }
  img.border = 0;
  link.insertBefore(img, link.firstChild);
}
