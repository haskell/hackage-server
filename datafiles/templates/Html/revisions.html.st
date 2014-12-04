<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Metadata revisions for $pkgid$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Metadata revisions for <a href="/package/$pkgid$">$pkgid$</a></h2>

<p>Package maintainers and Hackage trustees are allowed to edit certain bits
of package metadata after a release, without uploading a new tarball.
Note that the tarball itself is never changed, just the metadata that is
stored separately.

<table class="fancy" style="width: 100%">
  <tr>
    <th>No.</th>
    <th>Time</th>
    <th>User</th>
    <th>Changes</th>
  </tr>
  $revisions:{revision|
    <tr>
      <td valign="top"><a href="/package/$pkgid$/revision/$revision.number$.cabal">#$revision.number$</a></td>
      <td valign="top">$revision.time$</td>
      <td valign="top"><a href="/user/$revision.user$">$revision.user$</td>
      <td>
        <ul>
        $revision.changes:{change|<li><p>Changed $change.what$
                         from <pre>$change.from$</pre>
                         to <pre>$change.to$</pre></li>}$
        </ul>
      </td>
    </tr>
  }; separator=""$
</table>

</div>
</body></html>

