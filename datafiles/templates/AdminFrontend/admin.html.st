<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Admin front-end | Hackage</title>
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
    <p>Found accounts:</p>
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
  $elseif(findAccount)$
    <p>No matching accounts found</p>
  $endif$
</li>
<li><a href="/admin/accounts">Browse all accounts</a></li>
<li><a href="/admin/legacy">Browse accounts that still have legacy passwords</a></li>
<li><a href="/users/register">Create new account</a></li>
<li>Edit groups: <a href="/users/admins/edit">admin</a>,
  <a href="/packages/mirrorers/edit">mirrorers</a>,
  <a href="/packages/trustees/edit">trustees</a>,
  <a href="/packages/uploaders/edit">uploaders</a></li>
</ul>

<h2>Account signup and reset requests</h2>

<ul>
<li>
<form action="/admin" method="get">
  <button type="submit">Search for signup requests</button>
  <input type="text" name="find-signup" value="$findSignup$"/>
</form>
$if(first(signups))$
  <p>Found signup requests:</p>
  <table class="simpletable">
    <tr>
      <th>User name</th>
      <th>Real name</th>
      <th>Email</th>
      <th>Timestamp</th>
    </tr>
    $signups:{signup|
    <tr>
      <td>$signup.username$</td>
      <td>$signup.realname$</td>
      <td>$signup.email$</td>
      <td>$signup.timestamp$</td>
    </tr>
    }$
  </table>
$elseif(findSignup)$
  <p>No matching signup requests found</p>
$endif$
</li>
<li><a href="/admin/signups">Browse all signup requests</a></li>
<li><a href="/admin/resets">Browse all reset requests</a></li>
</ul>

<h2>Server status</h2>
<ul>
<li><a href="/server-status/memory">Memory use</a> by data store/cache</li>
</ul>


<h2>TODO</h2>

<p>TODO list for this admin interface
</p>
<ul>
<li>Account username change</li>
<li>Account undelete</li>
</ul>

</div> <!-- content -->
</body>
</html>
