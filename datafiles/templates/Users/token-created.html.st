<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Api key generated | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>New api key for $username$</h2>
<p>
  A new api key was successfully generated. Please keep it in a safe place and do
  not loose it. You can revoke it from the user management page.
</p>

<h3>API Key: $token$</h3>

<ul>
  <li>
    <a href="/user/$username$/manage">
      I've stored the key. Go back to management page.
    </a>
  </li>
</ul>

</div>
</body></html>
