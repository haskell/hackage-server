<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Set user notify preference | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Change notify preference</h2>

$if(showConfirmationOfSave)$
  <p class=box>
    Notify preference saved! The updated preference are shown below.
  </p>
$endif$

<form action="/user/$username$/notify" method=POST enctype="multipart/form-data">
  <input type="hidden" name="_method" value="PUT"/>
  <input type="hidden" name="_return" value="/user/$username$/notify?showConfirmationOfSave=True"/>
  <input type="hidden" name="_transform" value="form2json"/>
  <table>
    <tr>
      <td><label>Enabled:
      <td>$notifyEnabled$
    
    <tr>
      <td><label>Notify package revision range:
      <td>$notifyRevisionRange$

    <tr>
      <td><label>Notify package upload:
      <td>$notifyUpload$

    <tr>
      <td><label>Notify maintainer group change:
      <td>$notifyMaintainerGroup$

    <tr>
      <td><label>Notify docbuilder report:
      <td>$notifyDocBuilderReport$

    <tr>
      <td><label>Notify pending proposed tags:
      <td>$notifyPendingTags$
  </table>
  <input type="submit" value="Save notify preference" />
</form>
</div>
</body></html>
