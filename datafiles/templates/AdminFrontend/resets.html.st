<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Hackage: browse reset requests</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Admin front-end</h1>

<h2>Browse password reset requests</h2>

All currently active password reset requests (i.e. excluding expired and completed).

<table class="simpletable">
  <tr>
    <th>User name</th>
    <th>Timestamp</th>
  </tr>
  $resets:{reset|
  <tr>
    <td><a href="/admin/account/$reset.account.id$">$reset.account.name$</a></td>
    <td>$reset.timestamp$</td>
  </tr>
  }$
</table>

</div> <!-- content -->
</body>
</html>
