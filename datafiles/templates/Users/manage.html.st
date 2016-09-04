<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Manage your user account | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Manage user account $username$</h2>
<p>This site collects operations you can do to manage your user account</p>

<h3>API Keys</h3>
<p>
  You can register API authentication keys to use them to for example have services like continouos integration upload packages on your behalf without providing them your username and/or password.
</p>

<h4>Active keys</h4>
<ul>
$tokens:{token|
  <li>
    $token.description$
    <form action="/user/$username$/manage" method="post" enctype="multipart/form-data">
        <input type="hidden" name="action" value="revoke-auth-key" />
        <input type="hidden" name="auth-key" value="$token.hash$" />
        <input type="submit" value="Revoke this key" />
    </form>
  </li>
}$
</ul>
<h4>Register new key</h4>
<p>
  To register a new key please provide a description for it. After registering
  the key it will be shown to you once and you can not recover it after from
  the site. Please be sure to store it in a safe place as it currently has
  the same privileges as your username and password.
</p>
<form action="/user/$username$/manage" method="post" enctype="multipart/form-data">
  Description: <input type="text" name="description" /> <br />
  <input type="hidden" name="action" value="new-auth-key" />
  <input type="submit" value="Generate new key" />
</form>


<h3>Other options</h3>

<ul>
  <li>
    <a href="/user/$username$/password">Change your password</a>
  </li>
</ul>

</div>
</body></html>
