<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Uploading packages and package candidates | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Uploading packages</h2>

<div style="font-size: large; text-align: center;">Upload and publish
  a package <strong>permanently</strong>: <a href="/packages/upload">Upload</a></div>

<p>Uploading a package puts it in the <a href="/packages/browse">package index</a>
so that anyone can download it and view information about it. <strong>You can only
upload a package version once and this cannot be undone</strong>, so try to get it right the first time!
To reduce the risk of mistakes it's recommended to use the <a href="#candidates">package candidates feature</a> described below.
</p>

<p>Because each package added to the main package index has a cost of
operation and maintenance associated to it, <strong>your package should
strive to provide value for the community by being intended to be
useful to others</strong>, which entails giving your package a meaningful
synopsis/description as well as ensuring your package is installable
by helping with providing accurate meta-data.
</p>

<p>Packages must be in the form produced by Cabal's
<a href="https://cabal.readthedocs.io/en/latest/cabal-commands.html#cabal-v2-sdist">sdist</a> command:
a gzipped tar file <em>package</em>-<em>version</em><tt>.tar.gz</tt>
comprising a directory <em>package</em>-<em>version</em> containing a package
of that name and version, including <em>package</em><tt>.cabal</tt>.
See the notes at the bottom of the page.
</p>

<h3 id="versioning_and_curation">Package versioning and curation</h3>

<p>By default, uploaded packages are <i>curated</i> which means that both maintainers and hackage trustees may revise their metadata (particularly involving version bounds) to guide build tools in producing install-plans. (For more information on revisions, see the <a href="https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md">FAQ</A>).
  
<p>In order to ensure the integrity and well-functioning of the Hackage/Cabal ecosystem, all curated packages <abbr title="[RFC2119] The word 'should' is intended to denote that there may exist valid reasons in particular circumstances to ignore a particular item, but the full implications MUST be understood and carefully weighed before choosing a different course">should</abbr> follow Haskell's <a href="https://pvp.haskell.org/">Package Versioning Policy (PVP)</a>.</p>

<p>In particular, be aware that although the <a href="https://pvp.haskell.org/">PVP</a> and <a href="http://semver.org/">SemVer</a> are based on the same concepts they differ significantly in structure and consequently are <em>not compatible</em> with each other. Please consult the <a href="https://pvp.haskell.org/faq/#semver">PVP/SemVer FAQ section</a> for more details about the differences and related issues.</p>
  
<p>Further, an important property of the PVP contract is that it can only be effective and provide strong enough guarantees if it is followed not only by an individual package, but also by that package's transitive dependencies. Consequently, packages which are curated should aim to depend only on other curated packages.</p>

<p>In the course of the curation process, the
<a href="/packages/trustees">Hackage Trustees</a> need to be
able to contact package maintainers, to inform them about and help to resolve issues with
their packages (including its meta-data) which affect the Hackage
ecosystem.</p>
   
<p>Package uploaders may choose to exclude individual package  uploads from curation, by setting the <tt>x-curation:</tt> field of the package's cabal file to <tt>uncurated</tt>. Packages which are uncurated have no expectations on them regarding versioning policy.  Trustees or maintainers may <i>adopt</i> uncurated packages into the curated layer through metadata revisions. Metadata revisions must not set the value of the <tt>x-curation</tt> field to any variant of <tt>uncurated</tt>.</p>
    
<p>Two variants of the <tt>uncurated</tt> property are supported. First, <tt>uncurated-no-trustee-contact</tt>, which indicates that maintainers do not wish to be contacted by trustees regarding any metadata issues with the package. (Contact may still occur over issues that are not related to curation, such as licensing, etc.). Second, <tt>uncurated-seeking-adoption</tt>, which indicates that maintainers  would like their package to be adopted in the curated layer, but currently some issue prevents this, which they would like assistance with.</p>

<p>In the future, metadata regarding curation will be made available in the UI of Hackage, and different derived indexes will be provided for the uncurated and curated layers of packages.</p>  
 
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
    </ul></li>
</ol>
<p>Please make sure that you comply with the license of all code and other
material that you upload. For example, check that your tarball includes the
license files of any 3rd party code that you include.</p>

<h3>Privileges</h3>

<p>To upload a package, you'll need a Hackage <a href="/accounts">username</a>
and password.</p>

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

<h3>Group Accounts</h3>

<p>Occasionally organizations want to have a group / organizational account
for a package that is maintained by a group of people.  The recommended
approach for these cases is to only do package uploads from individual
accounts and use the group account only for managing the maintainer list for
the package.
</p>

<h3 id="candidates">Package Candidates</h3>

<p><a href="/packages/candidates">Package <em>candidates</em></a> are a way
to preview the package page, view any warnings or possible errors you might
encounter, and let others install it before publishing it to the main index.
(Note: you can view these warnings with 'cabal check'.) You can have multiple
candidates for each package at the same time so long as they each have different
versions. Finally, you can publish a candidate to the main index if it's not
there already.</p>

<p>Package candidates have not yet been fully implemented and are still being improved; see <a
href="https://github.com/haskell/hackage-server/projects/1">Package Candidates Project Dashboard</a> for an overview of what still needs to be done.</p>

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
