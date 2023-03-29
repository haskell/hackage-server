<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Set user notification preferences | Hackage</title>
<style>
td {
  vertical-align: top;
}
label {
  align-items: center;
}
.expandable {
  visibility: visible;
}
#notifyDependencyForMaintainedFalse:checked ~ .expandable {
  visibility: collapse;
}
</style>
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

    <tr>
      <td><label>Notify when a dependency is updated for any of my maintained packages:
      <td>
        <input $notifyDependencyForMaintainedTrueChecked$ type="radio" name="notifyDependencyForMaintained=%s" value="true" id="notifyDependencyForMaintainedTrue">
        <label for="notifyDependencyForMaintainedTrue">Yes</label>
        <input $notifyDependencyForMaintainedFalseChecked$ type="radio" name="notifyDependencyForMaintained=%s" value="false" id="notifyDependencyForMaintainedFalse">
        <label for="notifyDependencyForMaintainedFalse">No</label>
        <p class="expandable">
          trigger:
          <label style="display:flex">
            <input $notifyDependencyTriggerBoundsAlwaysChecked$ type="radio" name="notifyDependencyTriggerBounds=%s" value="Always">
            always
          </label>
          <label style="display:flex">
            <input $notifyDependencyTriggerBoundsBoundsOutOfRangeChecked$ type="radio" name="notifyDependencyTriggerBounds=%s" value="BoundsOutOfRange">
            my maintained package doesn't accept the version of the newly uploaded or revised dependency
          </label>
          <label style="display:flex">
            <input $newIncompatibilityChecked$ type="radio" name="notifyDependencyTriggerBounds=%s" value="NewIncompatibility">
            my maintained package doesn't accept the version of the newly uploaded or revised dependency
            and my package does accept the second highest version of said dependency
          </label>
        </p>
      </td>
    </tr>
  </table>
  <input type="submit" value="Save notify preference" />
</form>
</div>
</body></html>
