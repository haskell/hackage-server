<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Analytics pixels for $pkgname$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">

<h2>Adding a analytics pixel to <a href="/package/$pkgname$">$pkgname$</a></h2>

<p> 
  Configure an analytics pixel to be automatically loaded on your package’s page on Hackage. 
  You’ll need an image URL from any external analytics provider. This can be used to
   surface information about web traffic to your package including geographic 
  distribution, version distribution, and companies.
</p>

<form method="POST" class="box" action="/package/$pkgname$/analytics-pixels">
    <label for="analytics-pixel">Analytics Image URL</label>
    <input name="analytics-pixel" type="text" />
    <input type="submit" />
</form>

<h2>Existing analytics pixels for <a href="/package/$pkgname$">$pkgname$</a></h2>

<ul>
  $analyticsPixels:{analyticsPixel|
    <li>
        <form method="POST" action="/package/$pkgname$/analytics-pixels">
            <label for="analytics-pixel">$analyticsPixel$</label>
            <input type="hidden" name="analytics-pixel" value="$analyticsPixel$"/>
            <input type="hidden" name="_method" value="DELETE" />
            <input type="submit" value="Delete" />
        </form>
    </li>
  }; separator=""$
</ul>

</div>
</body>
</html>
