<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Auth token generated | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>New auth token for $username$</h2>
<p>
  A new token was successfully generated. Please keep it in a safe place and do
  not lose it. You can revoke it from the user management page.
</p>

<h3>Auth token: $token$</h3>

<ul>
  <li>
    <a href="/user/$username$/manage">
      I've stored the token. Go back to management page.
    </a>
  </li>
</ul>

</div>
</body></html>
