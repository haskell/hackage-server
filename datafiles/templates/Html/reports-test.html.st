<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Test settings</title>
</head>
<body>
$hackagePageHeader()$

<div id="content">
<h2>Test settings for $pkgid$</h2>

<form action="." method="post" enctype="multipart/form-data">

<dl>
<dt>Run tests</dt>
    <dd><input type="checkbox" name="runTests" id="runTests" $if(runTests)$checked$endif$>
        Whether hackage should run the tests.
    </dd>

<p><input type="submit" value="Save">
</form>

</div>
</body></html>
