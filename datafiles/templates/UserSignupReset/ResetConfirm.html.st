<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Account recovery | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Account recovery</h2>

<p>Email confirmation done!

<p>Now you can set a new password.

<form action="$posturl$" method="post" enctype="multipart/form-data">
  <table>
    <tr><td>Your name:<td>$realname$
    <tr><td>Login username:<td>$username$
    <tr><td>Contact email address:<td>$useremail$
    <tr><td><label for="password">Password</label>
        <td><input type="password" name="password" id="password">
    <tr><td><label for="repeat-password">Confirm Password</label>
        <td><input type="password" name="repeat-password" id="repeat-password">
  </table>

  <p><input type="submit" value="Set new password"/>
</form>

</div>
</body></html>

