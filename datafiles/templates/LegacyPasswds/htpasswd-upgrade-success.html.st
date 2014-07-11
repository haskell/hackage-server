<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Account upgrade successful | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Account upgrade successful</h2>
<p>Your account has been re-enabled.
</p>

<h3>Technical details for the curious</h3>

<p>The old hackage implementation used <a href="http://en.wikipedia.org/wiki/Basic_access_authentication">HTTP basic authentication</a>. The new system uses <a href="http://en.wikipedia.org/wiki/Digest_access_authentication">HTTP digest authentication</a>.

<p>We could not transparently upgrade accounts to the new system because
the password hash format is different for the new system. The old
format was the
<a href="http://httpd.apache.org/docs/2.2/misc/password_encryptions.html#basic">
Apache basic auth 'CRYPT' format</a>, while the new format is
equivalent to the
<a href="http://httpd.apache.org/docs/2.2/misc/password_encryptions.html#digest">
Apache digest authentication format</a>. It is not possible to generate
the new format without access to the plaintext password &ndash; which
was never stored. So by authenticating once using the old account
information &ndash; using HTTP basic authentication &ndash; we can
generate and store password digest for the new system.

</div>
</body></html>
