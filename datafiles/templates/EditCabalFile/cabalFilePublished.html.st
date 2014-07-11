<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Published new revision for $pkgid$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Published new revision for <a href="/package/$pkgid$">$pkgid$</a></h1>

<p>The new revision has been published. It will be available to users as soon
as they update their package index (e.g. <code>cabal update</code>).

<pre rows="20" cols="80">$cabalfile$</pre>

<h2>Changes in this revision</h2>
<ul>
$changes:{change|<li><p>Changed $change.what$
                 from <pre>$change.from$</pre>
                 to <pre>$change.to$</pre></li>}$
</ul>

<h2>Advice on adjusting version constraints</h2>

<p>You can edit the version constraints for the dependencies,
either to restrict or relax them. The goal in editing the constraints should
always be to make them reflect reality.
<ul>
<li><p>If the package fails to build against certain versions of a dependency
then constrain the version.
<li><p>If the package builds against <em>and work correctly</em> with a newer
version of a dependency then it is ok to relax the constraint
<ul>

</div>
</body></html>

