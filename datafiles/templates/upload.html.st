<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Uploading packages and package candidates | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Uploading packages and package candidates</h2>
<p>Uploading a package puts it in the <a href="/packages/browse">main package index</a>
so that anyone can download it and view information about it. You can only
upload a package version once, so try to get it right the first time!
</p>

<p>You can also upload a <a href="/packages/candidates">package <em>candidate</em></a>
to preview the package page, view any warnings or possible errors you might
encounter, and let others install it before publishing it to the main index.
(Note: you can view these warnings with 'cabal check'.) You can have multiple
candidates for each package at the same time so long as they each have different
versions. Finally, you can publish a candidate to the main index if it's not
there already.
</p>

<p>If you upload a package or package candidate and no other versions exist
in the package database, you become part of the maintainer group for that
package, and you can add other maintainers if you wish. If a maintainer group
exists for a package, only its members can upload new versions of that package.
</p>

<p>If there is no maintainer, the uploader can remove themselves from the group,
and a <a href="/packages/trustees">package trustee</a> can add anyone who wishes
to assume the responsibility. The <code>Maintainer</code> field of the Cabal file should be
<code>None</code> in this case. If a package is being maintained, any release not approved
and supported by the maintainer should use a different package name. Then use
the <code>Maintainer</code> field as above either to commit to supporting the fork
yourself or to mark it as unsupported.
</p>

<h3>Upload forms</h3>
<p>Some last formalities: to upload a package, you'll need a Hackage
<a href="accounts">username</a> and password. (Alternatively, there's a
command-line interface via cabal-install, which also needs the same username
and password.)
</p>

<p>Packages must be in the form produced by Cabal's
<a href="https://cabal.readthedocs.io/en/latest/cabal-commands.html#cabal-v2-sdist">sdist</a> command:
a gzipped tar file <em>package</em>-<em>version</em><code>.tar.gz</code>
comprising a directory <em>package</em>-<em>version</em> containing a package
of that name and version, including <em>package</em><code>.cabal</code>.
See the notes at the bottom of the page.
</p>

<div style="font-size: large; text-align: center;"><a href="/packages/upload">Upload a package</a></div>
<div style="font-size: large; text-align: center;"><a href="/packages/candidates/upload">Upload a package candidate</a></div>

<h3>Notes</h3>
<ul>

  <li>
    You should check that your source bundle builds, including the
    haddock documentation if it's a library.
  </li>

  <li>
    Categories are determined by whatever you put in the <code>Category</code> field.
    You should try to pick existing categories when possible.
    You can have more than one category, separated by commas. If no other versions of
    the package exist, the categories automatically become the package's tags.
  </li>

  <li>
    Occasional changes to the GHC base package can mean that some work needs to be done to make packages compatible across a range of versions.
    See <a href="https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md">these notes</a> for some tips in how to do so.

    There are some notes for upgrading <a href="http://www.haskell.org/haskellwiki/Upgrading_packages">much older</a> packages as well.
  </li>

  <li>
    The hackage-server attempts to build documentation for library
    packages, but this can fail. Maintainers can generate their own
    documentation and upload it by using something along the lines of the
    shell script below (note that the last two commands are the key ones):

    <pre>
#!/bin/sh
set -e

dir=\$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "\$dir"' EXIT

# assumes cabal 2.4 or later
cabal v2-haddock --builddir="\$dir" --haddock-for-hackage --enable-doc

cabal upload -d --publish \$dir/*-docs.tar.gz
</pre>
  </li>

</ul>

</div>
</body>
</html>
