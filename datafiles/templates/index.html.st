<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Hackage 2 public beta</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">

<h1>Hackage 2 public beta</h1>

<p>Well-Typed and the <a href="http://industry.haskell.org/">Industrial Haskell Group</a> (IHG) are very pleased to announce that Hackage 2 is now available for public beta testing. The plan is to do the final switchover in late September, to coincide with ICFP.</p><p>Read on for details of how to help with the public beta, an overview of the new features and what the IHG has been doing to help.</p>

<h2 id="support-from-the-industrial-haskell-group">Support from the Industrial Haskell Group</h2>

<a href="http://industry.haskell.org/"><img style="float: left; margin-right: 1em" src="http://industry.haskell.org/ihg-logo.png" height="90" alt="The IHG logo" /></a>

<p>The IHG is a consortium of companies that rely on Haskell. The IHG members have funded the effort to get Hackage 2 up to feature parity and get it ready for the switchover. The IHG funded this effort because while the volunteer effort got us the “first 90%” of the way there (including adding a number of new features) there was still the “last 90%” to do to get it production ready.</p>

<p>The IHG members decided to fund Hackage 2 not just because they are good citizens, but out of enlightened self-interest. Hackage has over 5000 packages written by over 1000 people — including the world’s best Haskell developers. This is a massive resource. The IHG members recognise that improvements to the tools and infrastructure that the community uses helps the community to produce more and better code. This is a benefit to everyone in the community — including the commercial users.</p>

<p>The IHG is keen to increase its membership so that more resources can be dedicated to improving the Haskell development platform. If your organisation relies on Haskell in some way then you may want to consider joining. See the <a href="http://industry.haskell.org/">IHG website</a> for more details or contact <a href="mailto:info@industry.haskell.org">info@industry.haskell.org</a>.</p>

<p>Despite the help of the IHG in getting to this point, Hackage is a community project, and its success depends on the community maintaining and further improving the new server. The code is now on github so it is easier to contribute, and now that the server is live there is more immediate gratification for volunteers contributing fixes and new features.</p>

<h2 id="public-beta">Public beta</h2>
<p>We would like to encourage you to take part in the public beta testing. We need help both from package authors as well as other users of the site.</p>

<p><a href="http://beta.hackage.haskell.org/">http://beta.hackage.haskell.org/</a></p>

<p>Please report any problems you find using the <a href="https://github.com/haskell/hackage-server/issues">issue tracker</a> on the <code>hackage-server</code> <a href="https://github.com/haskell/hackage-server">github site</a>.</p>

<p>We are mirroring packages from the old server (every 30min) so it is suitable to use as your main hackage server with some caveats: we are allowing package authors to upload (as well as doing the mirroring) so you may find a slightly different set of packages on this server.</p>

<p>If you are a package author then you are welcome to poke about and upload packages. We have imported user accounts from the old server (except for a small number of early adopters of the original server who will need to contact an administrator). Note that before we do the final switchover we will <strong>delete everything</strong> from the beta instance and do a fresh import from the old hackage server.</p>
  
<h3 id="configuring-cabal-install">Configuring cabal-install</h3>
<p>Edit your <code>~/.cabal/config</code> file. Comment-out the existing <code>remote-repo</code> line near the top of the file and add in a new one like this:</p>

<pre><code>--remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive

remote-repo: beta.hackage.haskell.org:http://beta.hackage.haskell.org/</code></pre>

<h2 id="new-features">New features</h2>
<p>Though our main priority has been feature parity so that we can switch over, volunteers have contributed several new features, including better package search, a new site theme, improved security, the ability to fix package dependencies after a release, changelogs, and a REST-style interface.</p>

<p>See the <a href="/new-features">new features</a> page for more details on these, plus details of other features that are partially implemented or are in need of improvement.</p>

<h2 id="contributing-to-the-development">Contributing to the development</h2>

<p>The <a href="https://github.com/haskell/hackage-server">code</a> is on github and we welcome pull requests.</p>

<p>There are open tickets describing existing bugs and features that we want or that are in need of improvement. Help on any of these would be greatly appreciated.</p>

<p>There is some developer and user documentation on the <a href="https://github.com/haskell/hackage-server/wiki">github wiki</a>, including a quick guide to getting your own server instance up and running.</p>
  
<p>You can ask questions on the <a href="http://www.haskell.org/pipermail/cabal-devel/">cabal-devel</a> mailing list or on IRC in the <code>#hackage</code> channel on freenode.</p>


<h2>Exploring the site</h2>

<p>The header bar on each page has the most important links:</p>
<ul>
<li>The <a href="/packages/search">Search</a> box lets you search for packages by keywords that appear in the package name or description.</li>
<li>The <a href="/packages/">Browse</a> page lists all the available packages.</li>
<li>The <a href="/recent">What's new</a> page lists recent additions (also available <a href="/recent.rss">as an RSS feed</a>).</li>
<li>The <a href="/upload">Upload</a> page lets you upload packages or package "candidates".</li>
</ul>

<p>In addition to the main package list page, there are a few other package indices:</p>
<ul>
<li><a href="/packages/tags">All tags</a></li>
<li><a href="/packages/names">All packages by name</a>, with tags</li>
<!-- <li><a href="/packages/reverse">All packages with reverse dependencies</a></li> -->
<li><a href="/packages/top">All packages by download</a></li>
<li><a href="/packages/preferred">All packages with preferred versions</a></li>
<li><a href="/packages/deprecated">All deprecated packages</a></li>
<li><a href="/packages/candidates">All candidate packages</a></li>
</ul>

<p>The new server has an automatically-generated <a href="/api">site api</a>. This is mainly intended as documentation for people working with the server, rather than as a user-oriented site map, but it can be interesting to see exactly what is available. This includes important resources like the machine-readable <a href="/packages/index.tar.gz">index of packages</a>, used by clients like <code>cabal-install</code>.</p>

</div>
</body>
</html>

