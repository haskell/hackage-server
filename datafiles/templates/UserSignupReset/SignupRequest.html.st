<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Register a new account | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Register a new account</h2>

<p>Certain actions on this website require you to have an account.
In particular you need an account to be able to upload or help maintain packages.

<p>Using the form below you can register an account.
The FirstnameLastname format of usernames is encouraged but not required.
Organizational, rather than individual, accounts are also permitted.

<form action="/users/register-request" method="post" enctype="multipart/form-data">

<table>
<tr>
<td><label for="realname">Your name</label>
<td><input type="text" name="realname" id="realname">
<td>This is what will be displayed on the site, e.g. Jan Novák

<tr>
<td><label for="username">Login username</label>
<td><input type="text" name="username" id="username">
<td>This has to be ASCII with no spaces or symbols (except '_'), e.g. JanNovak

<tr>
<td><label for="email">Your email address</label>
<td><input type="text" name="email" id="email">
<td>e.g. jnovak@example.com (but do <b>not</b> use the style "Jan Novák" &lt;jnovak@example.com&gt;)
</table>

<p>Your email address will be used to confirm your account (and if you ever
need to reset your password). It will also be used if one of the site
administrators ever needs to contact you. It will not be displayed on
the website (but note that email addresses in .cabal files that you
upload are public).

<p>Most contributors to Hackage attach their real name to their packages and
contributions. However, we have no wish to discourage contributors with
personal or professional reasons for wanting pseudonymity. We ask only that
such people choose a name (and username) that looks at home among a collection
of real names; we will be unwilling to add Kittenlover97 to the package
uploader group.

<p class="registration-email">
After your account is created, you cannot upload until you
<b>contact the hackage trustees</b> at hackage-trustees@haskell.org and
<b>send an email</b> (including your login username) requesting to be added to the uploader group.
(This measure is unfortunately necessary to prevent spam accounts).
</p>

<p><input type="submit" value="Request account">

<p>You will be sent an email containing a link to a page where you
can set your password and activate your account.

</form>

<h3>Open source licenses</h3>

<p>The code and other material you upload and distribute via this site must be
under an <em>open source license</em>. This is a service operated for the
benefit of the community and that is our policy. It is also so that we can
operate the service in compliance with copyright laws.</p>

<p>The Hackage operators do not want to be in the business of making judgements
on what is and is not a valid open source license, but we retain the right to
remove packages that are not under licenses that are open source in spirit, or
that conflict with our ability to operate this service. (If you want advice,
see the ones <a href="/package/Cabal/docs/Distribution-License.html">Cabal
recommends</a>.)</p>

<p>The Hackage operators do <em>not</em> need and are <em>not</em> asking for
any rights beyond those granted by the open source license you choose to use.
All normal open source licenses grant enough rights to be able to operate this
service.</p>

<p>In particular, we expect as a consequence of the license that:</p>
<ol>
<li>we have the right to distribute what you have uploaded to other people</li>
<li>we have the right to distribute certain derivatives and format conversions,
    including but not limited to:
    <ul>
      <li>documentation derived from the package</li>
      <li>alternative presentations and formats of code (e.g. html markup)</li>
      <li>excerpts and presentation of package metadata</li>
      <li>modified versions of package metadata</li>
    </ul>
</ol>

</div>
</body></html>
