<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Uploading packages and package candidates | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Upload a package</h2>

<div style="font-size: large; text-align: center;"><a href="/packages/upload">Upload a package</a></div>

<p>Uploading a package puts it in the <a href="/packages/">main package index</a>
so that anyone can download it and view information about it. You can only
upload a package version once, so try to get it right the first time!
Though you can try the package candidates feature below.
</p>

<p>Packages must be in the form produced by Cabal's
<a href="http://www.haskell.org/cabal/users-guide/installing-packages.html#setup-sdist">sdist</a> command:
a gzipped tar file <em>package</em>-<em>version</em><tt>.tar.gz</tt>
comprising a directory <em>package</em>-<em>version</em> containing a package
of that name and version, including <em>package</em><tt>.cabal</tt>.
See the notes at the bottom of the page.
</p>

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
<p>Please make sure that you comply with the license of all code and other
material that you upload. For example, check that your tarball includes the
license files of any 3rd party code that you include.</p>

<h3>Privileges</h3>

<p>To upload a package, you'll need a Hackage <a href="/accounts">username</a>
and password. </p>

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

<p>Note that all of the above is a makeshift upload policy based on the features
available in the newer hackage-server. The <code>Maintainer</code> field has its uses,
as does maintainer user groups. The libraries mailing list should probably
determine the best approach for this.
</p>

<h3>Package Candidates (Tech Preview)</h3>

<p><a href="/packages/candidates">Package <em>candidates</em></a> are a way 
to preview the package page, view any warnings or possible errors you might
encounter, and let others install it before publishing it to the main index.
(Note: you can view these warnings with 'cabal check'.) You can have multiple
candidates for each package at the same time so long as they each have different
versions. Finally, you can publish a candidate to the main index if it's not
there already.

<p>Package candidates have not yet been fully implemented; see <a
href="https://github.com/haskell/hackage-server/issues/41#issuecomment-25005344">Issue
41</a> for an overview of what still needs to be done.</p>

<div style="font-size: large; text-align: center;"><a href="/packages/candidates/upload">Upload a package candidate</a></div>

<h3>Notes</h3>
<ul>

  <li>
    You should check that your source bundle builds, including the
    haddock documentation if it's a library.
  </li>

  <li>
    Categories are determined by whatever you put in the <code>Category</code> field
    (there's no agreed list of category names yet).
    You can have more than one category, separated by commas. If no other versions of
    the package exist, the categories automatically become the package's tags.
  </li>

  <li>
    Documentation for library packages should be generated by a maintainer.
    The means of doing this is still up in the air.
  </li>

  <li>
    We have moved to Haddock 2, and expect some glitches.

    If you notice anything broken, please report it on the
    <a href="http://trac.haskell.org/haddock">Haddock bug tracker</a>.
  </li>

  <li>
    In GHC 6.8, several modules were split from the <code>base</code> package
    into other packages.

    See <a href="http://www.haskell.org/haskellwiki/Upgrading_packages">these notes</a>
    on making packages work with a range of versions of GHC.
  </li>

  <li>
    While <a href="http://www.haskell.org/haddock/">Haddock 2</a>
    accepts GHC features, it is also more picky about comment syntax than
    the old version.
  </li>

</ul>

</div>
</body>
</html>
