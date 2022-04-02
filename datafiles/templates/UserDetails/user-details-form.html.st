<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Set user details | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Change full name or e-mail address</h2>

$if(showConfirmationOfSave)$
  <p class=box>
  User details saved! The updated details are shown below.
  </p>
$endif$

<p>
  The email is used e.g. to contact you regarding the packages you maintain, or for account recovery.<br/>
  Make sure you have access to the new e-mail address, because <strong>no confirmation mail is sent</strong> when this form is submitted.
</p>

<form action="/user/$username$/name-contact" method=POST enctype="multipart/form-data">
  <input type="hidden" name="_method" value="PUT"/>
  <input type="hidden" name="_return" value="/user/$username$/name-contact?showConfirmationOfSave=True"/>
  <input type="hidden" name="_transform" value="form2json"/>
  <label>New full name: <input name="name=%s" value="$name$" /></label><br />
  <label>New e-mail address: <input type="email" name="contactEmailAddress=%s" value="$contactEmailAddress$" /></label><br />
  <input type="submit" value="Save user details" />
</form>
</div>
</body></html>
