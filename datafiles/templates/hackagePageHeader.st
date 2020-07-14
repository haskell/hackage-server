<div id="page-header">

  <a class="caption" href="/">Hackage :: [Package]</a>

<ul class="links" id="page-menu">

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
    $if(deauthPack)$
      <li><a title="If authenticated" href="/packages/deauth">Logout</a></li>
    $elseif(deauthAdmin)$
      <li><a title="If authenticated" href="/admin/deauth">Logout</a></li>
    $elseif(deauthUser)$
      <li><a title="If authenticated" href="./deauth">Logout</a></li>
    $endif$

</ul>

</div>
