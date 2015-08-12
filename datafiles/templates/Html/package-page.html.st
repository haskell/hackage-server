<!DOCTYPE html>
<html>
<head>
  $hackageCssTheme()$
  <title>
    $docTitle$
  </title>
  <!-- <link rel="canonical" href="http://localhost:8080/package/" $pkgName$ /> -->
</head>

<body>
  $hackagePageHeader()$

  <div id="content">

    <h1>The $pkgName$ package</h1>

    <div style="font-size: small">
      $headLinks$
    </div>

    $deprecatedMsg$
    <!-- Optional -->

    $pkgDescription$

    $skipToReadme$
    <!-- Optional -->

    <h2>Properties</h2>
    <table class="properties">
      <tbody>

        <tr>
          <th>Versions</th>
          <td>$versions$</td>
        </tr>

        <tr>
          <th>Change log</th>
          <td>$changelog$</td>
        </tr>

        <tr>
          <th>Dependencies</th>
          <td>$dependencies$</td>
        </tr>

        <tr>
          <th>License</th>
          <td>$license$</td>
        </tr>
        <tr>
          <th>Copyright</th>
          <td>$copyright$</td>
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
          <td>$stability$</td>
        </tr>
        <tr>
          <th>Category</th>
          <td>$categories$</td>
        </tr>
        <tr>
          <th>Home page</th>
          <td>$homePage$</td>
        </tr>
        <tr>
          <th>Bug tracker</th>
          <td>$bugTracker$</td>
        </tr>
        <tr>
          <th>Source repository</th>
          <td>$sourceRepo$</td>
        </tr>
        <tr>
          <th>Uploaded</th>
          <td>$uploadTime$</td>
        </tr>
        <tr>
          <th>Updated</th>
          <td>$updateTime$</td>
        </tr>
        <tr>
          <th>Distributions</th>
          <td>$distributions$</td>
        </tr>
        <tr>
          <th>Downloads</th>
          <td>$downloads$</td>
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

    $moduleList$

    $flags$

    $downloads$

    $maintainerOptions$

    $readme$

  </div> <!-- /content -->

  $footer()$

</body>
</html>
