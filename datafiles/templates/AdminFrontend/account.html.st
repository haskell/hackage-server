<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Hackage: browse user accounts</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Admin front-end</h1>

<p>$account$

<h2>Basic account information</h2>

<table>
  <tr><td>Username:<td>$account.name$
  <tr><td>User ID:<td>$account.id$
  <tr><td>Status:<td>$account.status$
</table>

$if(account.enabled)$
<p>The account is enabled for login.</p>

<p>
<form class="box" action="/user/$account.name$/enabled" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-PUT-form"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <button type="submit">Disable account</button>
<!--  <input type="hidden" name="enabled"/> -->
</form>
<form class="box" action="/user/$account.name$" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-DELETE"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <button type="submit">Delete account</button>
</form>
</p>

$elseif(account.hasAuth)$
<p>The account is disabled, so cannot currently be used for login.
The username is still reserved.
</p>
<p>The account still has a password set so it can be re-enabled.
</p>

<p>
<form class="box" action="/user/$account.name$/enabled" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-PUT-form"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <input type="hidden" name="enabled"/>
  <button type="submit">Enable account</button>
</form>
<form class="box" action="/user/$account.name$" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-DELETE"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <button type="submit">Delete account</button>
</form>
</p>
$elseif(account.active)$
<p>The account is disabled, so cannot currently be used for login.
The username is still reserved.
</p>
<p>The account does not have a password set. It will need a password to be set
before it can be re-enabled.
</p>

<form class="box" action="/user/$account.name$" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-DELETE"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <button type="submit">Delete account</button>
</form>
$else$
<p>The account has been marked as deleted, so cannot be used for login.
The username is available for reuse.
</p>
<p>The account can be undeleted if the username is not in use (and if you want to
enable it then the account will need a password to be set).
</p>

$endif$

<h2>Additional account information</h2>

<table>
  <tr><td>Real name:<td>$details.realname$
  <tr><td>Email:<td>$details.email$
  <tr><td>Account type:<td>$details.kind$
  <tr><td>Notes:<td>$details.notes$
</table>

<h2>TODO</h2>

<p>TODO list for this admin interface
<ul>
<li>Account username change</li>
<li>Account undelete</li>
<li>Account password set</li>
<li>Account password self-service reset</li>
<li>Legacy password</li>
<li>Edit extended information</li>
</ul>
</p>

</div> <!-- content -->
</body>
</html>
