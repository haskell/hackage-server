<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Register a new account | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Register a new account</h2>

<p>Certain actions on this website require you to have an account.
In particular you need an account to be able to upload or help maintain packages.

<p>Using the form below you can register an account.

<form action="/users/register-request" method="post" enctype="multipart/form-data">

<table>
<tr>
<td><label for="realname">Your name</label>
<td><input type="text" name="realname" id="realname">
<td>This is what will be displayed on the site, e.g. Jan Novák

<tr>
<td><label for="username">Login username</label>
<td><input type="text" name="username" id="username">
<td>This has to be ASCII with no spaces or symbols (except '_'), e.g. JanNovak

<tr>
<td><label for="email">Your email address</label>
<td><input type="text" name="email" id="email">
<td>e.g. jnovak@example.com (but do <b>not</b> use the style "Jan Novák" &lt;jnovak@example.com&gt;)
</table>

<p>Your email address will be used to confirm your account (and if you ever
need to reset your password). It will also be used if one of the site
administrators ever needs to contact you. It will not be displayed on
the website (but note that email addresses in .cabal files that you
upload are public).

<p><input type="submit" value="Request account">

<p>You will be sent an email containing a link to a page where you
can set your password and activate your account.
</form>

</div>
</body></html>
