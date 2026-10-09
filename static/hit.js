// Counts this page view; see "Readership" in the README.
if (location.hostname === "words.chreekat.net") {
  new Image().src = "https://hits.chreekat.net/hit?p=" + encodeURIComponent(location.pathname)
    + "&r=" + encodeURIComponent(document.referrer);
}
