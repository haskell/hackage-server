<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Browse signup requests | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Admin front-end</h1>

<h2>Browse account signup requests</h2>

All currently active account signup requests (i.e. excluding expired and completed).

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


</div> <!-- content -->
</body>
</html>
