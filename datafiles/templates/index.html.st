<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Introduction | Hackage</title>
<link rel="canonical" href="$sbaseurl$" />
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Hackage:  The Haskell Package Repository</h1>
<p><b>Hackage</b> is the <a href="http://www.haskell.org">Haskell</a>
   community's central package archive of open source software.
   Hackage has been online since January 2007 and is constantly
   growing.  You can <a href="/upload">publish</a> libraries and
   programs or download and install packages with tools
   like <a href="/package/cabal-install">cabal-install</a> (or via
   your distro's package manager).</p>

<h2>Package Downloads</h2>
<ul>
<li><a href="/packages/browse">Browse packages</a></li>
<li><a href="/packages/search">Search packages</a></li>
<li><a href="/packages/recent">Recent uploads</a></li>
</ul>

<p>Supplementary views:</p>
<ul>
<li><a href="/packages">Packages by category</a></li>
<li><a href="/packages/tags">Tags</a></li>
<!-- <li><a href="/packages/reverse">All packages with reverse dependencies</a></li> -->
<!-- <li><a href="/packages/top">All packages by download</a></li> -->
<li><a href="/packages/deprecated">Deprecated packages</a></li>
<li><a href="/packages/candidates">Candidate packages</a></li>
<li><a href="/packages/preferred">Preferred and deprecated</a> version information</li>
</ul>

<h2>Package Uploads</h2>
<ul>
<li><a href="/accounts">Register an account</a></li>
<li><a href="/upload">Upload a package</a></li>
</ul>

<p>Guidelines for Hackage Packages:</p>
<ul>
<li>Hackage accepts uploads
  of <a href="https://cabal.readthedocs.io/en/latest/intro.html#what-s-in-a-package">Cabal
    packages</a></li>
<li>Packages must be in the standard compressed tarball form produced by Cabal's
<a href="https://cabal.readthedocs.io/en/3.4/cabal-commands.html#cabal-v2-sdist">sdist</a>
command.</li>
<li><strong>Packages cannot be deleted</strong>, so you should consider uploading new package versions as <a href="/packages/candidates/upload">package candidates</a> and testing before publishing to the main index.</li>
<li>All <a href="/upload#versioning_and_curation">curated</a> packages <abbr title="[RFC2119] The word 'should' is intended to denote that there may exist valid reasons in particular circumstances to ignore a particular item, but the full implications MUST be understood and carefully weighed before choosing a different course">should</abbr> follow the <a href="https://pvp.haskell.org/">Package Versioning Policy (PVP)</a>.</li>
<li>Packages uploads may opt-out of curation through use of the <tt>x-curation</tt> field
<li>Please consult the <a href="/upload">documentation for uploading packages</a> for more in-depth information about Hackage's policies, including the usage of the <tt>x-curation</tt> field.</li>
</ul>

<h2>Administrative issues</h2>

<ul>
<li><a href="https://wiki.haskell.org/Taking_over_a_package">Taking over a package</a></li>
<li><a href="https://wiki.haskell.org/Abandoning_a_package">Abandoning a package</a></li>
<li><a href="https://wiki.haskell.org/Hackage_trustees">Hackage trustees</a> and what they do</li>
<li><a href="https://wiki.haskell.org/Library_submissions">Submitting  changes</a> for the core libraries</li>
</ul>

<h2 id="reporting-problems">Reporting problems</h2>

<p>For issues with accounts or permissions please contact the administrators by email
at <a href="mailto:hackage-admin@haskell.org">hackage-admin@haskell.org</a></p>

<p>Bugs with the site code or server/hosting issues should be reported
  in the <a href="https://github.com/haskell/hackage-server/issues">issue tracker</a>.</p>

<p>Scheduled infrastructure status information is available at <a href="http://status.haskell.org">status.haskell.org</a>
   and automated uptime information at <a href="http://auto-status.haskell.org/">auto-status.haskell.org</a>.
   Serious issues requiring immediate action should be reported to <a href="mailto:admin@haskell.org">admin@haskell.org</a> or on the
   #haskell-infrastructure irc channel on libera.chat.</p>

<h2 id="hackage-api">The Hackage API</h2>

 <p>Hackage serves most resources in JSON as well as HTML. It also
 provides automatically-generated documentation of
 the <a href="/api">site api</a>.  Core operations of clients
 interacting with hackage as a repository should be conducted through
 the <a href="http://hackage.haskell.org/package/hackage-security">hackage-security</a>
 library.</p>

<h2 id="contributing-todevelopment">Contributing to  development</h2>

<p>Hackage-server
  is <a href="https://github.com/haskell/hackage-server">on github</a>
  and we welcome contributions including pull requests, bug reports,
  and feature requests.  Developer documentation is on in the github
  README, includig a quick guide to running your own server instance,
  and mirroring the central server.  You can ask questions on
  the <a href="http://www.haskell.org/pipermail/cabal-devel/">cabal-devel</a>
  mailing list or on IRC in the <code>#hackage</code> channel on
  libera.chat.</p>

<h2>Credits</h2>
<p>The current Hackage codebase was spearheaded
 by <a href="https://www.well-typed.com/">Well-Typed</a>. Key funding
 was provided by the <a href="http://ihg.well-typed.com/">Industrial
 Haskell Group</a>, which was a consortium of companies that rely on Haskell.
 The website is maintained by volunteers from the
 Haskell community. Hosting is provided
 by <a href="https://www.packet.com/">Packet</a> and real-time
 CDN by <a href="https://www.fastly.com">Fastly</a>.</p>

</div>
</body>
</html>
