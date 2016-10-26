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

<h3>Authentication Tokens</h3>
<p>
  You can register API authentication token to use them to for example have services like continouos integration upload packages on your behalf without providing them your username and/or password.
</p>

<p>
  To authenticate using this token, you must set the <code>Authorization</code> header for the request you would like to authenticate:
</p>

<pre>
Authorization: X-ApiKey [YOUR-TOKEN-HERE]
</pre>

<p>
  You can try it out from the command line using curl:
</p>

<pre>curl --header "Authorization: X-ApiKey [YOUR-TOKEN-HERE]" [HACKAGE-TARGET-PAGE]</pre>

<h4>Active tokens</h4>
<ul>
$tokens:{token|
  <li>
    $token.description$
    <form action="/user/$username$/manage" method="post" enctype="multipart/form-data">
        <input type="hidden" name="action" value="revoke-auth-token" />
        <input type="hidden" name="auth-token" value="$token.hash$" />
        <input type="submit" value="Revoke this token" />
    </form>
  </li>
}$
</ul>
<h4>Register new token</h4>
<p>
  To register a new token please provide a description for it. After registering
  the token it will be shown to you once and you can not recover it after from
  the site. Please be sure to store it in a safe place as it currently has
  the same privileges as your username and password.
</p>
<form action="/user/$username$/manage" method="post" enctype="multipart/form-data">
  Description: <input type="text" name="description" /> <br />
  <input type="hidden" name="action" value="new-auth-token" />
  <input type="submit" value="Generate new token" />
</form>


<h3>Other options</h3>

<ul>
  <li>
    <a href="/user/$username$/password">Change your password</a>
  </li>
</ul>

</div>
</body></html>
