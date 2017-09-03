<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Introduction | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h1>Welcome to Hackage!</h1>
<p><b>Hackage</b> is the Haskell community's central package archive of open source software.
   Package authors use it to publish their libraries and programs while other
   Haskell programmers use tools like <a href="/package/cabal-install">cabal-install</a>
   to download and install packages (or people get the packages via their distro).</p>
<p>This web interface to Hackage lets you:</p>
<ul>
<li><a href="/packages">Browse</a> the packages (sorted by category)</li>
<li><a href="/packages/search">Search</a> for packages by keyword (in the name or description)</li>
<li>See what packages have been <a href="/packages/recent">uploaded recently</a></li>
<li><a href="/upload">Upload</a> your own packages to Hackage (note that you'll need an <a href="/accounts">account</a>)</li>
</ul>
<p>Each package includes:
<ul>
<li>A description of what it does</li>
<li>Licence information</li>
<li>Author information</li>
<li>A declarative specification of library & tool dependencies</li>
<li>A list of executables and/or library modules provided by the package</li>
<li>Haddock documentation (if available) with source links</li>
<li>A downloadable gzipped tarball</li>
</ul>
<p>Guidelines for Hackage Packages:</p>
<ul>
<li>All packages should follow the <a href="https://pvp.haskell.org/">Package Versioning Policy (PVP)</a>.</li>
<li><strong>Packages cannot be deleted</strong>, so you should consider uploading new versions packages as a <a href="/packages/candidates/upload">package candidate</a> and testing before publishing it to the main index.</li>
<li>Please consult the <a href="/upload">documentation for uploading packages</a> for more in-depth information about Hackage's policies.</li>
</ul>
<p>In addition to the main package list page, there are a few other package indices:</p>
<ul>
<li><a href="/packages/browse">Browsable package index</a></li>
<li><a href="/packages/tags">All tags</a></li>
<li><a href="/packages/names">All packages by name</a>, with tags</li>
<!-- <li><a href="/packages/reverse">All packages with reverse dependencies</a></li> -->
<li><a href="/packages/top">All packages by download</a></li>
<li><a href="/packages/preferred">All packages with preferred versions</a></li>
<li><a href="/packages/deprecated">All deprecated packages</a></li>
<li><a href="/packages/candidates">All candidate packages</a></li>
</ul>

<h1>Administrative issues</h1>

<ul>
<li><a href="https://wiki.haskell.org/Taking_over_a_package">Taking over a package</a> on Hackage</li>
<li><a href="https://wiki.haskell.org/Hackage_trustees">Hackage trustees</a> and what they do</li>
<li>Submitting <a href="https://wiki.haskell.org/Library_submissions">changes for the core libraries</a></li>
</ul>

<h2 id="reporting-problems">Reporting problems</h2>

<p>For issues with accounts or permissions please contact the administrators by email
at <a href="mailto:admin@hackage.haskell.org">admin@hackage.haskell.org</a></p>

<p>For bugs with the site code or server/hosting issues, please report them in our <a
href="https://github.com/haskell/hackage-server/issues">issue tracker</a>.

<h2 id="contributing-to-the-development">Contributing to the development</h2>

<p>The <a href="https://github.com/haskell/hackage-server">code</a> is on github and we welcome pull requests.</p>

<p>There are open tickets describing existing bugs and features that we want or that are in need of improvement. Help on any of these would be greatly appreciated.</p>

<p>There is some developer and user documentation on the <a href="https://github.com/haskell/hackage-server/wiki">github wiki</a>, including a quick guide to getting your own server instance up and running.</p>
  
<p>You can ask questions on the <a href="http://www.haskell.org/pipermail/cabal-devel/">cabal-devel</a> mailing list or on IRC in the <code>#hackage</code> channel on freenode.</p>

<p>The new server has an automatically-generated <a href="/api">site api</a>. This is mainly intended as documentation for people working with the server, rather than as a user-oriented site map, but it can be interesting to see exactly what is available. This includes important resources like the machine-readable <a href="/packages/index.tar.gz">index of packages</a>, used by clients like <code>cabal-install</code>.</p>


<h1>Hackage 2</h1>

<p>Well-Typed and the <a href="http://industry.haskell.org/">Industrial Haskell Group</a> (IHG) are very pleased to announce that Hackage 2 is now powering the official Hackage server.</p>

<p>Read on for an overview of the new features, system improvements, and for details of how you can help to make Hackage 2 even better.</p>

<h2 id="support-from-the-industrial-haskell-group">Support from the Industrial Haskell Group</h2>

<a href="http://industry.haskell.org/"><img style="float: left; margin-right: 1em" src="/static/ihg-logo.png" height="90" alt="The IHG logo" /></a>

<p>The IHG is a consortium of companies that rely on Haskell. The IHG members have funded the effort to get Hackage 2 up to feature parity and get it ready for the switchover. The IHG funded this effort because while the volunteer effort got us the “first 90%” of the way there (including adding a number of new features) there was still the “last 90%” to do to get it production ready.</p>

<p>The IHG members decided to fund Hackage 2 not just because they are good citizens, but out of enlightened self-interest. Hackage has over 5000 packages written by over 1000 people — including the world’s best Haskell developers. This is a massive resource. The IHG members recognise that improvements to the tools and infrastructure that the community uses helps the community to produce more and better code. This is a benefit to everyone in the community — including the commercial users.</p>

<p>The IHG is keen to increase its membership so that more resources can be dedicated to improving the Haskell development platform. If your organisation relies on Haskell in some way then you may want to consider joining. See the <a href="http://industry.haskell.org/">IHG website</a> for more details or contact <a href="mailto:info@industry.haskell.org">info@industry.haskell.org</a>.</p>

<p>Despite the help of the IHG in getting to this point, Hackage is a community project, and its success depends on the community maintaining and further improving the new server. The code is <a href="http://github.com/haskell/hackage-server">now on github</a> so it is easier to contribute, and now that the server is live there is more immediate gratification for volunteers contributing fixes and new features.</p>

<h2 id="support-from-the-smartest-cdn">The fastest Content Distribution Network</h2>

<a href="https://www.fastly.com/"><img style="float: left; margin-right: 1em" src="/static/fastly-logo.png" height="90" alt="Fastly's logo" /></a>

<p>As the needs of the Haskell.org community increase, Hackage and other parts of our infrastructure demand increasing amounts of bandwidth, lower latency, and more storage and compute resources. Hackage contains code written from people all over the world, and uptime as well as speed are paramount to a clean user experience when developing.</p>

<p><a href="https://www.fastly.com/">Fastly.com</a> has offered the Haskell.org community unlimited access to their CDN, allowing us to push TBs of data through their edge systems, provide resource shielding for servers, and get real-time analytics for all our users and data.</p>

<h2 id="new-features">New features</h2>
<p>Though our main priority has been feature parity so that we can switch over, volunteers have contributed several new features, including better package search, a new site theme, improved security, the ability to fix package dependencies after a release, changelogs, and a REST-style interface.</p>

<p>See the <a href="/new-features">new features</a> page for more details on these, plus details of other features that are partially implemented or are in need of improvement.</p>

</div>
</body>
</html>

