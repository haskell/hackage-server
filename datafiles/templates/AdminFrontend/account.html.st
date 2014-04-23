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

<h2>Account information</h2>

<table>
  <tr><td>Username:<td>$account.name$
  <tr><td>User ID:<td>$account.id$
  <tr><td>Status:<td>$account.status$
  <tr><td>Real name:<td>$details.realname$
  <tr><td>Email:<td>$details.email$
  <tr><td>Account type:<td>$details.kind$
  <tr><td>Notes:<td>$details.notes$
</table>

<h2>Account status</h2>

$if(account.enabled)$
<p>The account is enabled for login.</p>

<p>
<form class="box" action="/user/$account.name$/enabled" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-PUT-form"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <button type="submit">Disable account</button>
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
before it can be re-enabled. (Note that this must be set manually as the
self-service password reset can only be used with enabled accounts.)
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

<h2>Password reset</h2>

<p>
If the account has an associated email address <emph>and</emhp> the account is
already in the enabled state then you can issue a self-service password reset.
</p>

$if(details.canreset)$
<form class="box" action="/users/password-reset" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-POST-form"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <input type="hidden" name="username" value="$account.name$"/>
  <input type="hidden" name="email" value="$details.email$"/>

  <p><input type="submit" value="Issue password reset email" /></p>
</form>
<p>The user will be sent an email containing a link to a page where they
can set a new password.
</p>
$else$
This account is in a state where the the self-service password reset cannot be
used. The account must be enabled, and the account type must be "real user".
Of course the account also needs a valid email address for it to work.
$endif$

<h2>Password set</h2>
<p>
Alternatively, as an administrator you can set account passwords directly.
</p>

<form class="box" action="/user/$account.name$/password" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-PUT-form"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <table>
    <tr>
      <td><label for="password">Password</label></td>
      <td><input type="password" name="password" id="password" /></td>
    </tr>
    <tr>
      <td><label for="repeat-password">Confirm password</label></td>
      <td><input type="password" name="repeat-password" id="repeat-password" /></td>
    </tr>
  </table>
 
  <p><input type="submit" value="Change password" /></p>
</form>

<h2>Edit account details</h2>

<form class="box" action="/user/$account.name$/name-contact" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-PUT-form2json"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <table>
    <tr>
      <td><label for="realname">"Real" name</label></td>
      <td><input type="text" name="name=%s" id="realname" value="$details.realname$"/></td>
    </tr>
    <tr>
      <td><label for="email">Email</label></td>
      <td><input type="text" name="contactEmailAddress=%s" id="email" value="$details.email$" /></td>
    </tr>
  </table>
 
  <p><input type="submit" value="Update" /></p>
</form>

<form class="box" action="/user/$account.name$/admin-info" method="post" enctype="multipart/form-data">
  <input type="hidden" name="_method" value="wrap-PUT-form2json"/>
  <input type="hidden" name="_return" value="/admin/account/$account.id$"/>

  <table>
    <tr>
      <td><label>Account type</label></td>
      <td>
          $details.kindenum, ["Not set", "Real user account", "Special account"]:
          {enum, label|
          <div><input type="radio" name="accountKind=%v" id="$enum.asstring$" value="$enum.asjson$"
                      $if(enum.selected)$checked$endif$ />
               <label for="$enum.asstring$">$label$</label>
          </div>         
          }$
      </td>
    </tr>
    <tr>
      <td><label for="email">Notes</label></td>
      <td><textarea name="notes=%s" id="notes" rows=5 cols=40>$details.notes$</textarea></td>
    </tr>
  </table>
 
  <p><input type="submit" value="Update" /></p>
</form>


<h2>TODO</h2>

<p>TODO list for this admin interface
<ul>
<li>Account username change</li>
<li>Account undelete</li>
<li>Legacy passwords</li>
<li>View outstanding password reset requests</li>
<li>View outstanding account requests</li>
</ul>
</p>

</div> <!-- content -->
</body>
</html>
