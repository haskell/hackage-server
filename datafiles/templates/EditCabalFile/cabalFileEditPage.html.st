<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Edit package metadata for $pkgid$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Edit package metadata for $pkgid$ (Tech Preview)</h1>

<p><b>NOTE</b>: This is work in progress. It's not currently actually possible
to publish new revisions (see <a
href="https://github.com/haskell/hackage-server/issues/52">Issue 52</a>).</p>

<p>Package maintainers and Hackage trustees are allowed to edit certain bits
of package metadata after a release, without uploading a new tarball.

<form action="edit" method="post" enctype="multipart/form-data">
  <textarea autofocus style="font-family: monospace" name="cabalfile" rows="20" cols="80">$cabalfile$</textarea>
  <p><input type="submit" name="review" value="Review changes"/>
     <input type="submit" name="publish" value="Publish new revision"/>
     <a href="edit">Reset</a>

$if(publish)$
<p>Cannot publish new revision</p>
$endif$

$if(first(errors))$
<h2>Errors</h2>
$errors:{error|<p>$error$</p>}$
$endif$

$if(first(changes))$
<h2>Changes</h2>
<ul>
$changes:{change|<li><p>Changed $change.what$
                 from <pre>$change.from$</pre>
                 to <pre>$change.to$</pre></li>}$
</ul>
$elseif(publish)$
<h2>Errors</h2>
<p>No changes? A new revision isn't really a revision without any changes!</p>
$elseif(!first(errors))$
<h2>Changes</h2>
No changes.
$endif$
</form>

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

