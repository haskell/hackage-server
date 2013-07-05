<?xml version="1.0"?>
<OpenSearchDescription xmlns="http://a9.com/-/spec/opensearch/1.1/"
                       xmlns:moz="http://www.mozilla.org/2006/browser/search/">
<ShortName>Hackage</ShortName>
<Description>Search for Haskell packages on Hackage</Description>
<InputEncoding>UTF-8</InputEncoding>
<Image height="16" width="16" type="image/x-icon">$serverhost$/favicon.ico</Image>
<Url type="text/html" method="get"
     template="$serverhost$/packages/search?terms={searchTerms}" />
<!--
<Url type="application/x-suggestions+json" method="get"
     template="$serverhost$/packages/suggest.json?terms={searchTerms}" />
-->
<moz:SearchForm>$serverhost$/packages/search</moz:SearchForm>
</OpenSearchDescription>
