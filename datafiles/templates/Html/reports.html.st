<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Build reports for $pkgid$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Build reports for <a href="/package/$pkgid$">$pkgid$</a></h2>
<h5>Status: $status$</h5>
$if(first(reports))$
<table class="fancy" style="width: 100%">
  <tr>
    <th rowspan="2">No.</th>
    <th colspan="3" style="text-align: center">Outcome</th>
    <th rowspan="2">Compiler</th>
    <th rowspan="2">OS</th>
    <th rowspan="2">Arch</th>
    <th rowspan="2">Flags</th>
    <th rowspan="2">Details</th>
  </tr>
  <tr>
    <th>Install</th>
    <th>Docs</th>
    <th>Tests</th>
  </tr>
  $reports:{report|
    <tr>
      <td><a href="/package/$pkgid$/reports/$report.0$">#$report.0$</a></td>
      <td>$report.1.installOutcome$</td>
      <td>$report.1.docsOutcome$</td>
      <td>$report.1.testsOutcome$</td>
      <td>$report.1.compiler$</td>
      <td>$report.1.os$</td>
      <td>$report.1.arch$</td>
      <td>
        $if(first(report.1.flagAssignment))$
          $report.1.flagAssignment; separator=" "$
        $else$
          <i>none</i>
        $endif$
      </td>
      <td><a href="/package/$pkgid$/reports/$report.0$">More details</a></td>
    </tr>
  }; separator=""$
</table>
$else$
<p>No reports available.</p>
$endif$

</div>
</body></html>
