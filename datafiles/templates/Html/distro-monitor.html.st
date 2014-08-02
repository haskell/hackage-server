<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Tarballs for $pkgname$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Tarballs for $pkgname$</h2>

<p>The following versions of $pkgname$ exist:

<ul>
  $versions:{pkgid|<li><a href="/package/$pkgid$/$pkgid$.tar.gz">$pkgid$.tar.gz</a></li>}$
</ul>

</p>

</div>
</body></html>
