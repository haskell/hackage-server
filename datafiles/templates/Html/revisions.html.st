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
stored separately. For more information about metadata revisions, please
refer to the
<a href="https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md">Hackage Metadata Revisions FAQ</a>.
</p>

<table class="fancy" style="width: 100%">
  <tr>
    <th>No.</th>
    <th>Time</th>
    <th>User</th>
    <th>SHA256</th>
  </tr>
  $revisions:{revision|
    <tr>
      <td valign="top"><a href="/package/$pkgid$/revision/$revision.number$.cabal">-r$revision.number$</a></td>
      <td valign="top">$revision.htmltime$</td>
      <td valign="top"><a href="/user/$revision.user$">$revision.user$</td>
      <td valign="top">$revision.sha256$</th>
    </tr>
    <tr>
      <td valign="top"></td>
      <td colspan="3">
        <ul>
        $revision.changes:{change|<li><p>$change.what$
                         $if(change.from)$ $if(change.to)$from$endif$ <pre>$change.from$</pre>$endif$
                         $if(change.to)$ $if(change.from)$to$endif$ <pre>$change.to$</pre>$endif$</p></li>}$
        </ul>
      </td>
    </tr>
  }; separator=""$
</table>

</div>
</body></html>

