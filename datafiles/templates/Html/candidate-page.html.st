<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  $hackageCssTheme()$
  <title>
    $package.name$$if(package.optional.hasSynopsis)$: $package.optional.synopsis$$endif$
  </title>
  <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=PT+Sans:400,400i,700"/>
  <link rel="stylesheet" href="/static/hackage.css" type="text/css"/>
  <link rel="icon" href="/static/favicon.png" type="image/png"/>
  <meta name="viewport" content="width=device-width, initial-scale=1"/>
  <link rel="search" href="/packages/opensearch.xml" type="application/opensearchdescription+xml" title="Hackage"/>
  <link rel="canonical" href="/package/$package.name$"/>
</head>

<body>
  $hackagePageHeader()$
  <div id="content">
    <h1>$package.name$$if(package.optional.hasSynopsis)$: <small>$package.optional.synopsis$</small>$endif$</h1>

    $package.candidateBanner$
    <div style="font-size: small">
      [$maintainHtml$]
      [<a href="candidate/publish">Publish</a>]
    </div>

    $warningBox$

    <div id="description">
    $if(package.optional.hasDescription)$
    $package.optional.description$
    $endif$
    $if(package.optional.hasReadme)$
    <hr>
    [<a href="#readme">Skip to Readme</a>]
    $endif$
    </div>


    <h2>Properties</h2>
    <table class="properties">
        <tr>
          <th>Versions</th>
          <td>$versions$</td>
        </tr>

        <tr>
          $if(package.optional.hasChangelog)$
          <th>Change&nbsp;log</th>
          <td>$package.optional.changelog$</td>
          $else$
          <th>Change log</th>
          <td><strong class="warning">None available</strong></td>
          $endif$
        </tr>

        <tr>
          <th>Dependencies</th>
          <td>$package.buildDepends$</td>
        </tr>

        <tr>
          <th>License</th>
          <td>$package.license$</td>
        </tr>

        $if(package.optional.hasCopyright)$
        <tr>
          <th>Copyright</th>
          <td>$package.optional.copyright$</td>
        </tr>
        $endif$

        <tr>
          <th>Author</th>
          <td>$package.author$</td>
        </tr>
        <tr>
          <th>Maintainer</th>
          <td>$package.maintainer$</td>
        </tr>

        $if(package.optional.hasCategories)$
        <tr>
          <th>Category</th>
          <td>$package.optional.category$</td>
        </tr>
        $endif$

        $if(package.optional.hasHomePage)$
        <tr>
          <th>Home page</th>
          <td>
            <a href=$package.optional.homepage$>
              $package.optional.homepage$
            </a>
          </td>
        </tr>
        $endif$

        $if(package.optional.hasBugTracker)$
        <tr>
          <th>Bug&nbsp;tracker</th>
          <td>
            <a href="$package.optional.bugTracker$">
              $package.optional.bugTracker$
            </a>
          </td>
        </tr>
        $endif$

        $if(package.optional.hasSourceRepository)$
        <tr>
          <th>Source&nbsp;repo</th>
          <td>$package.optional.sourceRepository$</td>
        </tr>
        $endif$

        <tr>
          <th>Uploaded</th>
          <td>$hackage.uploadTime$</td>
        </tr>
    </table>
    $moduleList$

    $if(hackage.hasFlags)$
    $hackage.flagsSection$
    $endif$

    $downloadSection$

    <h4>Maintainer's Corner</h4>
    <p>Package maintainers</p>
    <ul>
      <li>
        $maintainers$
      </li>
    </ul>
    <p>For package maintainers and hackage trustees</p>
    <ul>
      <li>
        <a href="candidate/maintain">
          edit package information
        </a>
      </li>
    </ul>

    $if(package.optional.hasReadme)$
    <hr/>
    <h2 id="readme">Readme for $package.name$-$package.version$</h2>
    [<a href="#description">back to package description</a>]
    $package.optional.readme$
    $endif$

</body>
</html>
