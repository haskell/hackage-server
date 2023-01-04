<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Set user notification preferences | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Change notification preferences</h2>

$if(showConfirmationOfSave)$
  <p class=box>
    Notification preferences saved! The updated preferences are shown below.
  </p>
$endif$

<form action="/user/$username$/notify" method=POST enctype="multipart/form-data">
  <input type="hidden" name="_method" value="PUT"/>
  <input type="hidden" name="_return" value="/user/$username$/notify?showConfirmationOfSave=True"/>
  <input type="hidden" name="_transform" value="form2json"/>
  <table>
    <tr>
      <td><label>Email notification enabled:
      <td>$notifyEnabled$

    <tr>
      <td><label>Notify on maintained package metadata revision:
      <td>$notifyRevisionRange$

    <tr>
      <td><label>Notify on maintained package upload:
      <td>$notifyUpload$

    <tr>
      <td><label>Notify on maintained package maintainer group change:
      <td>$notifyMaintainerGroup$

    <tr>
      <td><label>Notify on maintained package docbuilder report:
      <td>$notifyDocBuilderReport$

    <tr>
      <td><label>Notify on maintained package pending proposed tags:
      <td>$notifyPendingTags$
  </table>
  <input type="submit" value="Save notify preference" />
</form>
</div>
</body></html>
