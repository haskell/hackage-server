<!DOCTYPE html>
<html>
<head>
  $if(doc.hasQuickNavV1)$
  <link href="$doc.baseUrl$/quick-jump.css" rel="stylesheet" type="text/css" title="QuickJump" />
  $endif$
  $hackageCssTheme()$
  <title>
    $package.name$$if(package.optional.hasSynopsis)$: $package.optional.synopsis$$endif$
  </title>
  <link rel="canonical" href="/package/$package.name$" />
  <script src="/static/jquery.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML" type="text/javascript"></script>
  <base href="/package/$package.name$-$package.version$/" />
</head>

<body>
  $hackagePageHeader()$
  <div id="content">
    <h1><a href="/package/$package.name$">$package.name$</a>$if(package.optional.hasSynopsis)$: <small>$package.optional.synopsis$</small>$endif$</h1>

    <div class="candidate-info">
      $maintainHtml$
    </div>
    <div style="font-size: small">
      [<a href="/package/$package.name$/candidate/maintain">maintain</a>] 
    </div>
    <div class="candidate-warn">
      $warningBox$
    </div>

    <div id="description">
    $if(package.optional.hasDescription)$
    $package.optional.description$
    $endif$
    $if(package.optional.hasReadme)$
    <hr>
    [<a href="#readme">Skip to Readme</a>]
    $endif$
    </div>


    <div id="properties">
    <table class="properties">
      <tbody>

        <tr>
          <th>Versions <span style="font-weight:normal;font-size: small;">[<a href="/package/$package.name$.rss">RSS</a>]</span></th>
          <td>$versions$</td>
        </tr>

        $if(package.optional.hasChangelog)$
        <tr>
          <th>Change&nbsp;log</th>
          <td class="word-wrap">$package.optional.changelog$</td>
        </tr>
        $endif$

        <tr>
          <th>Dependencies</th>
          <td>$package.buildDepends$</td>
        </tr>

        <tr>
          <th>License</th>
          <td class="word-wrap">$package.license$</td>
        </tr>

        $if(package.optional.hasCopyright)$
        <tr>
          <th>Copyright</th>
          <td class="word-wrap">$package.optional.copyright$</td>
        </tr>
        $endif$

        <tr>
          <th>Author</th>
          <td class="word-wrap">$package.author$</td>
        </tr>
        <tr>
          <th>Maintainer</th>
          <td class="word-wrap">$package.maintainer$</td>
        </tr>

        $if(hackage.hasUpdateTime)$
        <tr>
          <th>Revised</th>
          <td>$hackage.updateTime$</td>
        </tr>
        $endif$

        <!-- Obsolete/deprecated 'Stability' field hidden
             c.f. http://stackoverflow.com/questions/3841218/conventions-for-stability-field-of-cabal-packages
        <tr>
          <th>Stability</th>
          <td>$stability$</td>
        </tr>
        -->

        $if(package.optional.hasCategories)$
        <tr>
          <th>Category</th>
          <td>$package.optional.category$</td>
        </tr>
        $endif$

        $if(package.optional.hasHomePage)$
        <tr>
          <th>Home page</th>
          <td class="word-wrap">
            <a href=$package.optional.homepage$>
              $package.optional.homepage$
            </a>
          </td>
        </tr>
        $endif$

        $if(package.optional.hasBugTracker)$
        <tr>
          <th>Bug&nbsp;tracker</th>
          <td class="word-wrap">
            <a href="$package.optional.bugTracker$">
              $package.optional.bugTracker$
            </a>
          </td>
        </tr>
        $endif$

        $if(package.optional.hasSourceRepository)$
        <tr>
          <th>Source&nbsp;repo</th>
          <td class="word-wrap">$package.optional.sourceRepository$</td>
        </tr>
        $endif$

        <tr>
          <th>Uploaded</th>
          <td>$hackage.uploadTime$</td>
        </tr>

    <div id="modules">
      $moduleList$
    </div>

    $if(hackage.hasFlags)$
    <div id="flags">
      $hackage.flagsSection$
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
          <a href="candidate/maintain">
            edit package information
          </a>
        </li>
      </ul>
    </div>

    $if(package.optional.hasReadme)$
    <hr />
    <div id="readme-container">
      <h2 id="readme">Readme for $package.name$-$package.version$</h2>
      [<a href="#description">back to package description</a>]
      $package.optional.readme$
    </div>
    $endif$
  </div> <!-- /content -->

  $packagePageAssets()$
  <div style="clear:both"></div>
  $footer()$

  <!-- $if(doc.hasQuickNavV1)$
  <script src="$doc.baseUrl$/quick-jump.min.js" type="text/javascript"></script>
  <script type="text/javascript"> quickNav.init("$doc.baseUrl$", function(toggle) {var t = document.getElementById('quickjump-trigger');if (t) {t.onclick = function(e) { e.preventDefault(); toggle(); };}}); </script>
  $endif$ -->
</body>
</html>