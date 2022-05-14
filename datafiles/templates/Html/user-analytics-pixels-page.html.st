<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Analytics pixels for all of $username$'s packages | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">

<h2>Create a analytics pixel</h2>

<p> 
  Configure an analytics pixel to be automatically loaded on your package’s page on Hackage. 
  You’ll need an image URL from any external analytics provider. This can be used to
  surface information about web traffic to your package including geographic 
  distribution, version distribution, and companies.
</p>

<form method="POST" class="box" action="/user/$username$/analytics-pixels">
    <p>    
        <label for="package">Package</label>
        <select name="package">
        $pkgs:{pkg|
            <option value="$pkg$">$pkg$</option>      
        }; separator=""$
        </select>
    </p>
    <p>
        <label for="analytics-pixel">Analytics pixel URL</label>
        <input name="analytics-pixel" type="text" />
    </p>
    <input type="submit" />
</form>

<h2>Existing analytics pixels</h2>

$pkgpixels:{pkgpixel|
<h3><a href="/package/$pkgpixel.0$">$pkgpixel.0$</a></h3>
<ul>
  $pkgpixel.1:{analyticsPixel|
    <li>
        <form method="POST" action="/user/$username$/analytics-pixels">
            <label for="analytics-pixel">$analyticsPixel$</label>
            <input type="hidden" name="package" value="$pkgpixel.0$" />
            <input type="hidden" name="analytics-pixel" value="$analyticsPixel$"/>
            <input type="hidden" name="_method" value="DELETE" />
            <input type="submit" value="Delete" />
        <form>
    </li>
  }; separator=""$
</ul>
}; separator=""$

</div>
</body>
</html>
