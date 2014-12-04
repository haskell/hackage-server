<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>User accounts with legacy passwords | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Admin front-end</h1>

<h2>User accounts with legacy passwords</h2>

<p>All user accounts with legacy "htpasswd" passwords set:<p>

$if(first(accounts))$
<ul>
$accounts:{account|
  <li>
    $if(account.active)$
    <a href="/admin/account/$account.id$#passwd-legacy">$account.name$</a>
    $else$
    <del><a href="/admin/account/$account.id$#passwd-legacy">$account.name$</a></del> (deleted)
    $endif$
  </li>
}$
</ul>
$else$
<p>None.</p>
$endif$

</div> <!-- content -->
</body>
</html>
