<div id="page-header">

<ul class="links" id="page-menu">

    <li><a href="/">Home</a></li>

    <li>
      <form action="/packages/search" method="get" class="search">
        <button type="submit">Search&nbsp;</button>
        <input type="text" name="terms" />
      </form>
    </li>

    <li><a href="/packages/browse">Browse</a></li>

    <li><a href="/packages/recent">What's new</a></li>

    <li><a href="/upload">Upload</a></li>

    <li><a href="/accounts">User accounts</a></li>

</ul>

$if(title)$
$title$
</div>
$else$
<a class="caption" href="/">Hackage :: [Package]</a>
$endif$

</div>
