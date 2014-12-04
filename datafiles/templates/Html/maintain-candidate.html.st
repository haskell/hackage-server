<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Maintainers' page for $pkgname$-$pkgversion$ candidate | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Maintainers' page for $pkgname$-$pkgversion$ candidate</h2>

<p>Here, you can delete a candidate, publish it, upload a new one, and
edit the maintainer group.

<dl>
<dt><a href="delete">Delete candidate</a></dt>
  <dd>Discard this candidate (does not affect published packages).
  </dd>

<dt><a href="publish">Publish candidate</a></dt>
  <dd>Publish this candidate to make it visible in the main package database.
  </dd>

<dt><a href="upload">Upload a new candidate</a></dt>
  <dd>If you upload a new candidate with the same version as an
  existing candidate, the older will be overwritten.
  </dd>

<dt><a href="/package/$pkgname$/maintainers">Maintainer group</a></dt>
  <dd>Only these users are allowed to upload new versions of the package.
      Existing members can add other users into the maintainer group.
  </dd>

</dl>

</div>
</body></html>
