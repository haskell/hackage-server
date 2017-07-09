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
  $changes:{change|<li><p>$change.what$
                         $if(change.from)$ $if(change.to)$from$endif$ <pre>$change.from$</pre>$endif$
                         $if(change.to)$ $if(change.from)$to$endif$ <pre>$change.to$</pre>$endif$</p></li>}$
</ul>

<h2>Advice on adjusting version constraints</h2>

<p>You can edit the version constraints for the dependencies,
either to restrict or relax them. The goal in editing the constraints should
always be to make them reflect reality.
<ul>
<li><p>If the package is incompatible with or fails to build against certain versions of a dependency
then constrain the version.
<li><p>If the package builds against <em>and works correctly (i.e. is compatible)</em> with a newer
version of a dependency then it is ok to relax the constraint
<li><p>See also the Hackage Trustee's <a href="https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md#best-practice-for-managing-meta-data">recommendations for managing meta-data</a></li>
<ul>

</div>
</body></html>

