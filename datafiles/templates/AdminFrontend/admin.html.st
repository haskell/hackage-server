<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Hackage: admin front-end</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Admin front-end</h1>

<h2>User accounts</h2>
<ul>
<li>
  <form action="/admin" method="get">
    <button type="submit">Search for account</button>
    <input type="text" name="find-account" value="$findAccount$"/>
  </form>
  $if(first(accounts))$
    <p>Found accounts:
      <ul>
      $accounts:{account|
        <li>
          $if(account.active)$
          <a href="/admin/account/$account.id$">$account.name$</a>
          $else$
          <del><a href="/admin/account/$account.id$">$account.name$</a></del> (deleted)
          $endif$
        </li>
      }$
      </ul>
    </p>
  $elseif(findAccount)$
    <p>No matching accounts found</p>
  $endif$
</li>
<li><a href="/admin/accounts">Browse all accounts</li>
<li><a href="/users/register">Create new account</a></li>
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

<h2>Server status</h2>
<ul>
<li><a href="/server-status/memory">Memory use</a> by data store/cache</a></li>
</ul>

</div> <!-- content -->
</body>
</html>
