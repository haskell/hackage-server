<!DOCTYPE html>
<html>
<head>
  $hackageCssTheme()$
  <title>
    $package.name$$if(package.optional.hasSynopsis)$: $package.optional.synopsis$$endif$
  </title>
  <link rel="canonical" href="$baseurl$/package/$package.name$" />
  <script src="https://code.jquery.com/jquery-1.12.4.min.js"></script>
    <style>
      .cool {
          color: gold;
      }
      .uncool {
          color: black;
      }
      .star-rating {
          margin:0;
          list-style-type: none;
          font-size: 150%;
          color: black;
      }
      .star-rating li {
          float: left;
          margin: 0 1% 0 1%;
      }
    </style>
</head>

<body>
  $hackagePageHeader(title={
    <a class="caption" href="$baseurl$/package/$package.name$">
    $package.name$$if(package.optional.hasSynopsis)$: $package.optional.synopsis$$endif$
    </a>})$

  <div id="content">

    <h1>The $package.name$ package</h1>
    <div style="font-size: small">
      [<a href="/packages/tags">Tags: </a>$tags$]
      <a href="/package/$package.name$/tags/edit">[ Propose Tags ]</a>
    </div>

    $if(isDeprecated)$
    <div id="deprecated">
      Deprecated.
      $deprecatedMsg$
    </div>
    $endif$

    $if(package.optional.hasDescription)$
    $package.optional.description$
    $endif$

    $if(package.optional.hasReadme)$
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

        $if(package.optional.hasChangelog)$
        <tr>
          <th>Change log</th>
          <td>$package.optional.changelog$</td>
        </tr>
        $endif$

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

        <tr>
          <th>Stability</th>
          <td>$stability$</td>
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
          <th>Bug tracker</th>
          <td>
            <a href="$package.optional.bugTracker$">
              $package.optional.bugTracker$
            </a>
          </td>
        </tr>
        $endif$

        $if(package.optional.hasSourceRepository)$
        <tr>
          <th>Source repository</th>
          <td>$package.optional.sourceRepository$</td>
        </tr>
        $endif$

        <tr>
          <th>Uploaded</th>
          <td>$hackage.uploadTime$</td>
        </tr>

        $if(hackage.hasUpdateTime)$
        <tr>
          <th>Updated</th>
          <td>$hackage.updateTime$</td>
        </tr>
        $endif$

        $if(hackage.hasDistributions)$
        <tr>
          <th>Distributions</th>
          <td>$hackage.distributions$</td>
        </tr>
        $endif$

        $if(hasrdeps)$
        <tr>
          <th>ReverseDependencies</th>
          <td>$rdeps$</td>
        </tr>
        $endif$

        $if(hasexecs)$
        <tr>
          <th>Executables</th>
          <td>$executables$</td>
        </tr>
        $endif$

        <tr>
          <th>Downloads</th>
          <td>$totalDownloads$ total ($recentDownloads$ in the last 30 days)</td>
        </tr>

        <tr>
          <th>Rating</th>
          <td>$score$ ($votes$ votes)
          $if(myrating)$
            <div style="font-size: small">
                [my rating : $myrating$]
                [<a href="" class="clear-rating">Clear Rating</a>]
            </div>
          $endif$
            <ul class="star-rating">
                <li class="star" id="1">&lambda;</li>
                <li class="star" id="2">&lambda;</li>
                <li class="star" id="3">&lambda;</li>
            </ul>
          </td>
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
          <a href="$package.name$/maintain">
            edit package information
          </a>
        </li>
      </ul>
    </div>

    $if(package.optional.hasReadme)$
    <div id="readme-container">
      <h2 id="readme">Readme for $package.name$</h2>
      $package.optional.readme$
    </div>
    $endif$

  </div> <!-- /content -->

  $footer()$

</body>
<script>
        var url = '/package/$package.name$/votes';
        var star = {
            "selected": false
        };
        \$('.star').mouseenter(function() {
            if (star.selected === false) {
                fill_stars(this.id, "in");
            }
        });
        \$('.star').mouseleave(function() {
            if (star.selected === false) {
                fill_stars(this.id, "out");
            }
        });
        \$('.star').click(function() {
            fill_stars(5, "out");
            fill_stars(this.id, "in");
            star.selected = true;
            star.id = this.id;
            var formData = {score : this.id}
            \$.post(url, formData).done(function (data){});
        });
        \$('.clear-rating').click(function(e) {
            e.preventDefault()
            fill_stars(5, "out");
            star.selected = false;
            \$.ajax({
            url: url,
            type: 'DELETE',
            // success: function(result) {}
        });
        });

        var fill_stars = function(num, direction){
            if (direction === "in")
                for (i = 0; i <= parseInt(num); i++)
                    \$("#" + i).removeClass('uncool').addClass('cool');
            else
                for (i = 0; i <= parseInt(num); i++)
                    \$("#" + i).removeClass('cool').addClass('uncool');
        }
    </script>
</html>
