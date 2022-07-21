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

<p>
  The email is used e.g. to contact you regarding the packages you maintain, or for account recovery.<br/>
  Make sure you have access to the new e-mail address, because <strong>no confirmation mail is sent</strong> when this form is submitted.
</p>

<form action="/user/$username$/notify" method=POST enctype="multipart/form-data">
  <input type="hidden" name="_method" value="PUT"/>
  <input type="hidden" name="_return" value="/user/$username$/notify?showConfirmationOfSave=True"/>
  <input type="hidden" name="_transform" value="form2json"/>
  <label>Enabled: $notifyEnabled$ </label><br />
  <label>Notify Revision Range: $notifyRevisionRange$ </label><br />
  <label>Notify Upload: $notifyUpload$ </label><br />
  <label>Notify Maintainer Group: $notifyMaintainerGroup$ </label><br />
  <label>Notify DocBuilder Report: $notifyDocBuilderReport$ </label><br />
  <label>Notify Pending Tags: $notifyPendingTags$ </label><br />
  <input type="submit" value="Save notify preference" />
</form>
</div>
</body></html>
