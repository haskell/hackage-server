<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Maintainers' page for $pkgname$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Maintainers' page for $pkgname$</h2>

<p>Package maintainers (and Hackage trustees) can edit a few things about the
package after its been released.

<dl>
<dt><a href="tags/edit">Package tags</a></dt>
  <dd>Package tags are used to improve search results and related packages to
      each other.
  </dd>

<dt><a href="preferred/edit">Preferred versions</a></dt>
  <dd>If you want users to keep using an older version when you release a newer
      version then you can set a preferred version range and tools like cabal
      will take it into account.
      You can also use this mechanism to deprecate individual versions (e.g. if
      you know they are broken) without deprecating the whole package.
  </dd>

<dt><a href="deprecated/edit">Deprecation</a></dt>
  <dd>You can deprecate the whole package (optionally in favour of some other
      package).
  </dd>

<dt><a href="maintainers">Maintainer group</a></dt>
  <dd>Only these users are allowed to upload new versions of the package.
      Existing members can add other users into the maintainer group.
  </dd>

<dt>Cabal file metadata</dt>
  <dd>You can edit certain bits of package metadata after a release, without uploading a new tarball.
  <p>$versions:{pkgid|<a href="/package/$pkgid$/$pkgname$.cabal/edit">$pkgid$</a>}; separator=", "$</p>
  </dd>

</div>
</body></html>
