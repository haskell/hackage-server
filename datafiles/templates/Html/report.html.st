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

<table class="fancy">
  <tr>
    <th>Package</th>
    <td>$report.1.package$</td>
  </tr>
</table>

<table class="fancy">
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

<table class="fancy">
  <tr>
    <th>Time submitted</th>
    <td>
      $if(report.1.time)$
        $report.1.time$
      $else$
        <i>unknown</i>
      $endif$
    </td>
  </tr>
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

<h3>Code Coverage</h3>

$if(covg)$
  <table class="fancy">
    <tr><th colspan="2">expressions</th><td>$covg.0.0$% ($covg.0.1$/$covg.0.2$)</td></tr>
    <tr><th colspan="1" rowspan="3">boolean</th><th>guards</th><td>$covg.1.0$% ($covg.1.1$/$covg.1.2$)    </td></tr>
    <tr><th>conditions</th><td> $covg.2.0$% ($covg.2.1$/$covg.2.2$) </td></tr>
    <tr><th>qualifiers</th><td>$covg.3.0$% ($covg.3.1$/$covg.3.2$)</td></tr>
    <tr><th colspan="2">alternatives</th><td>$covg.4.0$% ($covg.4.1$/$covg.4.2$)</td></tr>
    <tr><th colspan="2">local declarations</th><td>$covg.5.0$% ($covg.5.1$/$covg.5.2$)</td></tr>
    <tr><th colspan="2">top-level declarations</th><td>$covg.6.0$% ($covg.6.1$/$covg.6.2$)</td></tr>
  </table>
$else$
  <p>No Code Coverage was submitted for this report.</p>
$endif$

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
