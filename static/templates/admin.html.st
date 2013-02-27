<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>HackageDB: user administartion</title>
</head>

<body>
$hackagePageHeader()$

<div class="content">
<h2>Admin front-end</h2>

<ul>
<li><a href="/users/register">Add user</a></li>
<li>Edit groups: <a href="/users/admins/edit">admin</a>,
  <a href="/packages/mirrorers/edit">mirrorers</a>,
  <a href="/packages/trustees/edit">trustees</a>,
  <a href="/packages/uploaders/edit">uploaders</a></li>
</ul>

<!--
<H3>Change Password</H3>

<FORM CLASS="box" action="/admin/users/change-password" method="post" enctype="multipart/form-data">
<P>User name: <INPUT type="text" name="user-name"></P>
<P>Password:  <INPUT type="password" name="password"></P>
<P>Repeat password: <INPUT type="password" name="repeat-password"></P>
<INPUT type="submit" value="Change Password">
</FORM>

<H3>Disable User</H3>

<FORM CLASS="box" action="/admin/users/disable" method="post" enctype="multipart/form-data">
<P>User name: <INPUT type="text" name="user-name"></P>
<INPUT type="submit" value="Disable user">
</FORM>


<H3>Re-Enable User</H3>

<FORM CLASS="box" action="/admin/users/enable" method="post" enctype="multipart/form-data">
<P>User name: <INPUT type="text" name="user-name"></P>
<INPUT type="submit" value="Enable user">
</FORM>

<H3>Delete User</H3>

<P>THIS IS PERMANENT</P>
<FORM CLASS="box" action="/admin/users/delete" method="post" enctype="multipart/form-data">
<P>User name: <INPUT type="text" name="user-name"></P>
<INPUT type="submit" value="Delete user">
</FORM>

  -->

</div> <!-- content -->
</body>
</html>
