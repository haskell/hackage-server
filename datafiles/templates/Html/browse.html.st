<!DOCTYPE html>
<html lang=en>
  <head>
    $hackageCssTheme()$
    <meta charset="UTF-8" />
    <title>$heading$ | Hackage</title>
    <style>
    #searchQuery {
      width: 50vw;
      margin-left: 0;
    }
    #browseTable th a {
      display:block;
      color: inherit;
      text-decoration: inherit;
      padding-right: 20px;
    }
    .commaList {
      display: inline;
      list-style: none;
      margin: 0;
    }
    .commaList li {
      display: inline;
    }
    .commaList li::after {
      content: ", ";
    }
    .commaList li:last-child::after {
      content: "";
    }
    #browseTable {
      width: 100%;
      margin-top: 3em;
    }
    #browseTable th, #browseTable td {
      padding: 4px 17px 4px 4px;
    }
    #browseTable th {
      border-bottom: 1px solid #111;
    }
    #browseTable th {
      background-repeat: no-repeat;
      background-image: url(/static/images/sort_both.png);
      background-position: center right;
      padding-right: 0;
    }
    #browseTable th.ascending {
      background-image: url(/static/images/sort_asc.png);
    }
    #browseTable th.descending {
      background-image: url(/static/images/sort_desc.png);
    }
    .paginator {
      margin-left: auto;
    }
    .paginator a {
      box-sizing: border-box;
      display: inline-block;
      min-width: 1.5em;
      padding: 0.5em 1em;
      margin-left: 2px;
      text-align: center;
      text-decoration: none !important;
      color: #333 !important;
      border: 1px solid transparent;
      border-radius: 2px;
    }
    .paginator .current, .paginator .current:hover {
      color: #333 !important;
      border: 1px solid #979797;
      background: linear-gradient(to bottom, #fff 0%, #dcdcdc 100%);
    }
    .paginator a:hover {
      color: white !important;
      border: 1px solid #111;
      background: linear-gradient(to bottom, #585858 0%, #111 100%);
    }
    .paginator span {
      padding: 0 1em;
      cursor: default;
    }
    .paginator .disabled {
      color: #666;
    }
    .filterSuggestion {
      display: flex;
      align-items: center;
      margin-top: 1em;
    }
    .filterSuggestion > input {
      margin-left: 0;
    }
    .filterSuggestion > div {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
    }
    .filterSuggestion > div > span {
      margin-right: .5em;
    }
    .filterSuggestion > div > input {
      margin: 0;
    }
    #paginatorContainer {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
    }
    #fatalError {
      display: none;
      color: red;
      font-weight: bold;
    }
    #arrow-name {
      width: 100px;
    }
    #arrow-downloads, #arrow-rating {
      width: 50px;
    }
    #arrow-description {
      min-width: 160px;
    }
    #arrow-tags {
      width: 140px;
    }
    #arrow-lastUpload {
      width: 80px;
    }
    #arrow-maintainers {
      width: 100px;
    }
    #arrow-packageRank {
      width: 150px;
    }
    .lastUpload, #sliderAndOutput {
      white-space: nowrap;
    }
    #browseFooter {
      display: none;
    }
    #toggleAdvanced {
      margin: 0.7em 0 0.7em 0;
    }
    #toggleAdvanced a {
      text-decoration: inherit;
      color: inherit;
    }
    #advancedForm {
      display: none;
      margin-left: 2em;
    }
    #advRatingSlider {
      vertical-align: middle;
    }
    input:invalid {
      outline: none;
      border-color: #edca9e;
      box-shadow: 0 0 10px #edca9e;
    }
    dt {
      font-family: monospace;
    }
    </style>
    <noscript>
    <style>
    .javaScriptOnly {
      display: none;
    }
    </style>
    </noscript>
  </head>
  <body>
    $hackagePageHeader()$
    <div id=content>
      <h1>$heading$</h1>
      <noscript>
        $formFragment$
        <!-- Elinks does not understand CSS, so it will show the full page.
             We add this warning such that users will not use the wrong form.
             The class attribute hides the notice for Firefox/Chrome users.
         -->
        <p class=javaScriptOnly>Please disregard the following form, it only works with JavaScript.</p>
      </noscript>
      <div class=javaScriptOnly>
        <div id=fatalError></div>
        <form onsubmit="javascript: submitSearch(event)">
        <input type=text id=searchQuery />
        <input type=submit value="Search" />
        </form>
        <h3 id=toggleAdvanced>
          <a aria-expanded=false aria-controls=advancedForm href="javascript: toggleAdvanced();">
            <span id=chevron>&#x25B8;</span>
            Advanced options
          </a>
        </h3>
        <div id=advancedForm>
        <form class=filterSuggestion onsubmit="javascript:appendDeprecated(event);"><input type=submit value=Append> <div>Also show deprecated packages</div></form>
        <form class=filterSuggestion onsubmit="javascript:appendAgeOfLastUL(event);"><input type=submit value=Append><div><span>Last uploaded version younger than</span><input placeholder="e.g. 2y for 2 years" id=advAgeLastUL oninput="validateAgeOfLastUL();"></div></form>
        <form class=filterSuggestion onsubmit="javascript:appendTag(event);"><input type=submit value=Append>        <div><span>Only show packages with tag</span><input id=advTag placeholder="e.g. bsd3" oninput="validateTag();"></div></form>
        <form class=filterSuggestion onsubmit="javascript:appendRating(event);"><input type=submit value=Append>     <div><span>Rating greater than, or equal to</span><span id=sliderAndOutput><input id=advRatingSlider type=range min=0 max=3 step=.1 oninput="this.nextElementSibling.value = this.value" value=2><output>2</output></span></div></form>
        <h5>Usage</h5>
        <p>Apart from just writing words to search everywhere in package metadata,
        you can also use filters in the search query string input field above.
        Filters are surrounded by parentheses.
        All filters have to pass for every package shown in the result,
        that is, it is a
        <a href="https://en.wikipedia.org/wiki/Logical_conjunction" target=_blank>
        logical conjunction</a>.
        </p>
        <dl>
          <dt>(downloads > 1000)</dt>
          <dd>Only show packages with more than 1000 downloads within the last 30 days. The download count is inexact because Hackage uses a <a href="https://en.wikipedia.org/wiki/Content_delivery_network" target=_blank>content delivery network</a>.</dd>
          <dt>(lastUpload &lt; 2021-10-29)</dt>
          <dd>Only show packages for which the last upload was before (i.e. excluding) the given UTC date in <a target=_blank href="https://www.w3.org/TR/NOTE-datetime">the 'complete date' format as specified using ISO 8601</a>.</dd>
          <dt>(lastUpload = 2021-10-29)</dt>
          <dd>Only show packages for which the last upload was within the 24 hours of the given UTC date.</dd>
          <dt>(maintainer:SimonMarlow)</dt>
          <dd>Only show packages for which the maintainers list includes the user name <a target=_blank href="/user/SimonMarlow">SimonMarlow</a>.</dd>
          <dt>(tag:bsd3)</dt>
          <dd>Only show packages with the <code><a target=_blank href="/packages/tag/bsd3">bsd3</a></code> tag.</dd>
          <dt>(not tag:network)</dt>
          <dd>Do not show packages with the <code><a target=_blank href="/packages/tag/network">network</a></code> tag. The <code>not</code> operator can also be used with other filters.</dd>
          <dt>(ageOfLastUpload > 5d)</dt>
          <dd>Only show packages uploaded more than five days ago.</dd>
          <dt>(ageOfLastUpload > 4w)</dt>
          <dd>Only show packages uploaded more than four weeks ago. A week has seven days.</dd>
          <dt>(ageOfLastUpload &lt; 1m)</dt>
          <dd>Only show packages last uploaded less than one month ago. A month is considered to have 30.437 days.</dd>
          <dt>(ageOfLastUpload &lt; 2.5y)</dt>
          <dd>Only show packages last uploaded less than 2.5 years ago. A year is considered to be 365.25 days.</dd>
          <dt>(rating > 2.5)</dt>
          <dd>Only show packages with a rating of more than 2.5. The dot is the only accepted decimal separator.</dd>
          <dt>(rating /= 0)</dt>
          <dd>Only show packages with a rating unequal to zero.</dd>
          <dt>(deprecated:any)</dt>
          <dd>Do not filter out deprecated packages. This must be explicitly added if desired.</dd>
          <dt>(deprecated:true)</dt>
          <dd>Only show deprecated packages.</dd>
          <dt>(deprecated:false)</dt>
          <dd>Only show packages that are not deprecated. If no other deprecation filter is given, this filter is automatically added.</dd>
          <dt>(distro:Debian)</dt>
          <dd>Only show packages that are available in the Debian distribution. See the <a href="/distros">full list of available distributions</a>.</dd>
        </dl>
        </div>
        <table id=browseTable class=fancy>
          <thead>
            <tr>
              <th id=arrow-name><a href="javascript: sort('name')">Name</a></th>
              <th id=arrow-downloads title="Over the last 30 days"><a href="javascript: sort('downloads')">DLs</a></th>
              <th id=arrow-rating title="Ranges from 0 to 3"><a href="javascript: sort('rating')">Rating</a></th>
              <th id=arrow-description><a href="javascript: sort('description')">Description</a></th>
              <th id=arrow-tags><a href="javascript: sort('tags')">Tags</a></th>
              <th id=arrow-lastUpload><a href="javascript: sort('lastUpload')">Last U/L</a></th>
              <th id=arrow-maintainers><a href="javascript: sort('maintainers')">Maintainers</a></th>
              <th id=arrow-packageRank><a href="javascript: sort('packageRank')">Package Rank</a></th>
            </tr>
          </thead>
          <tbody id="listing"></tbody>
        </table>
        <script type=module>
        import { sort, submitSearch, toggleAdvanced, appendDeprecated, appendAgeOfLastUL,
                 appendTag, appendRating, validateAgeOfLastUL, validateTag }
               from "/static/browse.js";
        window.sort = sort;
        window.submitSearch = submitSearch;
        window.toggleAdvanced = toggleAdvanced;
        window.appendDeprecated = appendDeprecated;
        window.appendAgeOfLastUL = appendAgeOfLastUL;
        window.appendTag = appendTag;
        window.appendRating = appendRating;
        window.validateAgeOfLastUL = validateAgeOfLastUL;
        window.validateTag = validateTag;
        </script>
        <div id=paginatorContainer>
        </div>
        <div id=browseFooter>
        Alternatively, if you are looking for a particular function then try <a href="#" id=hoogleLink target=_blank>Hoogle</a>.
        </div>
      </div>
    </div>
  </body>
</html>
