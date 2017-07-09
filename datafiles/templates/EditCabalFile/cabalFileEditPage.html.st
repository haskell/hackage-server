<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Edit package metadata for $pkgid$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Edit package metadata for $pkgid$</h1>

<p>Package maintainers and Hackage trustees are allowed to edit certain bits
of package metadata after a release, without uploading a new tarball.
Note that the tarball itself is never changed, just the metadata that is
stored separately.

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
  $changes:{change|<li><p>$change.what$
                         $if(change.from)$ $if(change.to)$from$endif$ <pre>$change.from$</pre>$endif$
                         $if(change.to)$ $if(change.from)$to$endif$ <pre>$change.to$</pre>$endif$</p></li>}$
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
<li><p>If the package is incompatible with or fails to build against certain versions of a dependency
then constrain the version.
<li><p>If the package builds against <em>and works correctly (i.e. is compatible)</em> with a newer
version of a dependency then it is ok to relax the constraint
<li><p>See also the Hackage Trustee's <a href="https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md#best-practice-for-managing-meta-data">recommendations for managing meta-data</a></li>
<ul>

</div>
</body></html>

