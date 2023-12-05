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
will be added to the uploaders group, and allowed to upload packages. Only endorse people who you trust to upload packages responsibly.</p>
<ul>
  $vouches$
</ul>

</div>
</body></html>
