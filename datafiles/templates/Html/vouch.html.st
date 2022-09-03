<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Vouch for user | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Vouch for user</h2>

<p>$msg$</p>

<form action="" method=POST>
<input type=submit value="Vouch for this user">
</form>

<p>Vouching cannot be undone! When the user has three vouches, the user
can upload packages. Note that users are, to a certain degree, held accountable
for the actions of the users they vouch for. Only vouch for people you know.</p>

<ul>
  $vouches$
</ul>

</div>
</body></html>
