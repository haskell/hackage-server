
<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Hackage: Manage documentation for $pkgid$</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Manage documentation for $pkgid$</h1>

<h2>Upload documentation</h2>
<p>Hackage usually builds package documentation automatically. However, if you wish, you may also build and upload the HTML yourself.</p>
<p>For more information, see the Notes section of the <a href="/upload">Upload page</a>.</p>
<form class="box" method="post" action="/package/$pkgid$/$actionUrl$docs" enctype="multipart/form-data">
  <input type="file" name="_file" />
  <input type="hidden" name="_method" value="PUT" />
  <input type="hidden" name="_transform" value="file2raw" />
  <input type="submit" value="Upload documentation" />
</form>

<h2>Delete documentation</h2>
<form class="box" method="post" action="/package/$pkgid$/$actionUrl$docs" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="DELETE" />
  <input type="submit" value="Delete documentation" />
</form>

</div>
</body>
</html>
