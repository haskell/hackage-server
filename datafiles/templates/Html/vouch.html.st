<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Endorse user | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Endorse user</h2>

<p>$msg$</p>

<form action="" method=POST>
<input type=submit value="Endorse this user">
</form>

<p>Endorsing cannot be undone! When the user has $requiredNumber$ endorsements, the user
can upload packages. Note that users are, to a certain degree, held accountable
for the actions of the users they endorse. Only endorse people you know.</p>

<ul>
  $vouches$
</ul>

</div>
</body></html>
