<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Hackage: Build #$report.0$ for $pkgid$</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Build #$report.0$ for <a href="/package/$pkgid$">$pkgid$</a></h2>

<p style="font-size: small">[<a href="/package/$pkgid$/reports">all reports</a>]</p>

<table class="properties">
  <tr>
    <th>Package</th>
    <td>$report.1.package$</td>
  </tr>
</table>

<table class="properties">
  <tr>
    <th>Install</th>
    <td>$report.1.installOutcome$</td>
  </tr>
  <tr>
    <th>Docs</th>
    <td>$report.1.docsOutcome$</td>
  </tr>
  <tr>
    <th>Tests</th>
    <td>$report.1.testsOutcome$</td>
  </tr>
</table>

<table class="properties">
  <tr>
    <th>Compiler</th>
    <td>$report.1.compiler$</td>
  </tr>
  <tr>
    <th>OS</th>
    <td>$report.1.os$</td>
  </tr>
  <tr>
    <th>Arch</th>
    <td>$report.1.arch$</td>
  </tr>
  <tr>
    <th>Dependencies</th>
    <td>$report.1.dependencies; separator=", "$</td>
  </tr>
  <tr>
    <th>Flags</th>
    <td>
      $if(first(report.1.flagAssignment))$
        $report.1.flagAssignment; separator=" "$
      $else$
        <i>none</i>
      $endif$
    </td>
  </tr>
</table>

<h3>Build log</h3>

$if(log)$
<p style="font-size: small">[<a href="/package/$pkgid$/reports/$report.0$/log">view raw</a>]</p>
<pre>
$log$</pre>
$else$
<p>No log was submitted for this report.</p>
$endif$

</div>
</body></html>
