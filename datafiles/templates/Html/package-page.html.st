<!DOCTYPE html>
<html>
<head>
  $hackageCssTheme()$
  <title>
    $docTitle$
  </title>
  <link rel="canonical" href="$baseurl$/package/$pkgName$" />
</head>

<body>
  $hackagePageHeader()$

  <div id="content">

    <h1>The $pkgName$ package</h1>
    <div style="font-size: small">
      [<a href="/packages/tags">Tags</a>:$tags$]
      $headLinks$
    </div>

    <!-- Optional -->
    $if(isDeprecated)$
    <div id="deprecated">
      Deprecated.
      $deprecatedMsg$
    </div>
    $endif$
    <!-- /Optional -->

    $pkgDescription$

    $if(package.hasReadme)$
    <hr>
    [<a href="#readme">Skip to Readme</a>]
    $endif$

    <div id="properties">
    <h2>Properties</h2>
    <table class="properties">
      <tbody>

        <tr>
          <th>Versions</th>
          <td>$versions$</td>
        </tr>

        <tr>
          <th>Change log</th>
          <td>$package.changelog$</td>
        </tr>

        <tr>
          <th>Dependencies</th>
          <td>$dependencyList$</td>
        </tr>

        <tr>
          <th>License</th>
          <td>$license$</td>
        </tr>
        <tr>

          <th>Copyright</th>
          <td>$package.copyright$</td>
        </tr>
        <tr>
          <th>Author</th>
          <td>$author$</td>
        </tr>
        <tr>
          <th>Maintainer</th>
          <td>$maintainer$</td>
        </tr>
        <tr>
          <th>Stability</th>
          <td>$package.stability$</td>
        </tr>
        <tr>
          <th>Category</th>
          <td>$categories$</td>
        </tr>

        $if(package.hasHomePage)$
        <tr>
          <th>Home page</th>
          <td>
            <a href=$package.homePageUrl$>$package.homePageUrl$</a>
          </td>
        </tr>
        $endif$

        $if(package.hasBugTracker)$
        <tr>
          <th>Bug tracker</th>
          <td>
            <a href="$package.bugTracker$">$package.bugTracker$</a>
          </td>
        </tr>
        $endif$

        <tr>
          <th>Source repository</th>
          <td>$sourceRepo$</td>
        </tr>
        <tr>
          <th>Uploaded</th>
          <td>$uploadTime$</td>
        </tr>

        $if(package.hasUpdateTime)$
        <tr>
          <th>Updated</th>
          <td>$package.updateTime$</td>
        </tr>
        $endif$

        $if(package.hasDistributions)$
        <tr>
          <th>Distributions</th>
          <td>$distributions$</td>
        </tr>
        $endif$

        <tr>
          <th>Downloads</th>
          <td>$totalDownloads$ total ($recentDownloads$ in the last 30 days)</td>
        </tr>

        <tr>
          <th>Votes</th>
          <td>$votes$</td>
        </tr>
        <tr>
          <th>Status</th>
          <td>$buildStatus$</td>
        </tr>
      </tbody>
    </table>
    </div> <!-- /properties -->

    <div id="modules">
      $moduleList$
    </div>

    $if(package.hasFlags)$
    <div id="flags">
      $package.flags$
    </div>
    $endif$

    <div id="downloads">
      $downloadSection$
    </div>

    <div id="maintainer-corner">
      <h4>Maintainer's Corner</h4>
      <p>For package maintainers and hackage trustees</p>
      <ul>
        <li>
          <a href="$pkgName$/maintain">edit package information</a>
        </li>
      </ul>
    </div>

    $if(package.hasReadme)$
    <div id="readme-container">
      <h2 id="readme">Readme for $pkgName$</h2>
      $package.readme$
    </div>
    $endif$

  </div> <!-- /content -->

  $footer()$

</body>
</html>
