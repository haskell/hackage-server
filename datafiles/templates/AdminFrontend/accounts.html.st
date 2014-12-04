<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Browse user accounts | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Admin front-end</h1>

<h2>Browse user accounts</h2>

<p>All user accounts, including disabled and deleted.</p>

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

</div> <!-- content -->
</body>
</html>
