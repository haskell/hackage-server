<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Account recovery | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Account recovery for forgotten passwords</h2>

<p>If you have forgotten the password you use to log in to Hackage, you can
recover the account and set a new password.

<p>Enter your account name and the email address that you <em>originally</em>
signed up with. The system will send you an email with a link that you can use
to set a new password.

<form action="/users/password-reset" method="post" enctype="multipart/form-data">

<table>
<tr>
<td><label for="username">Login username</label>
<td><input type="text" name="username" id="username">

<tr>
<td><label for="email">Your email address</label>
<td><input type="text" name="email" id="email">
<td>This must be the same email address that you registered with originally. 
</table>

<p><input type="submit" value="Request account recovery">

<p>You will be sent an email containing a link to a page where you
can set a new password.
</form>

</div>
</body></html>
