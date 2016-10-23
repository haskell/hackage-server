<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Introduction | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>About Hackage</h2>

<p>Hackage is a collection of released
<a href="http://www.haskell.org/">Haskell</a> packages.
Each package is in the <a href="http://www.haskell.org/cabal">Cabal</a> format,
a standard way of packaging Haskell source code that makes it easy to build
and install.

These pages are a basic web interface to the Hackage package database.
</p>

  <p>This isn't the <em>official</em> Hackage, which is at
  <a href="http://hackage.haskell.org">http://hackage.haskell.org</a>.
  Rather, this is being run either for development purposes or by a third-party.
</p>

<h3>Finding packages</h3>

<p>The <em>Packages</em> link above lists the available packages
and provides a full text search of the package pages (via Google),
while <em>What's new</em> lists recent additions (also available
<a href="/recent.rss">as an RSS feed</a>). You can also do a simple text
search of package descriptions:
</p>

<form action="/packages/search" method="get">
<p>
<input name="terms"/>
<input type="submit" value="Search"/>
</p>
</form>

<p>There are a few other package indices:</p>
<ul>
<li><a href="/packages/browse">Browsable package index</a></li>
<li><a href="/packages/tags">All tags</a></li>
<li><a href="/packages/names">All packages by name</a>, with tags</li>
<li><a href="/packages/reverse">All packages with reverse dependencies</a></li>
<li><a href="/packages/top">All packages by download</a> (since old download data isn't imported, this will look somewhat barren)</li>
<li><a href="/packages/preferred">All packages with preferred versions</a></li>
<li><a href="/packages/deprecated">All deprecated packages</a></li>
<li><a href="/packages/candidates">All candidate packages</a></li>
</ul>

<p>See <a href="http://www.haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package">How to install a Cabal package</a> for instructions on installing the packages you find here.</p>

<h3>Releasing packages through Hackage</h3>
<p>To upload your own releases, you'll first need to package them as Cabal
source packages.
See <a href="http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program">How to create a Haskell package</a>
for a tutorial.
You can check and upload your package using the <em>Upload</em> link above,
though you'll need a Hackage <a href="accounts.html">username</a> and password.
</p>

<h3>Getting the raw data</h3>
<ul>
<li><a href="/packages/index.tar.gz">tarball of package descriptions</a></li>
</ul>

<h3>Development</h3>
<p>See the
<a href="https://github.com/haskell/hackage-server/wiki">Hackage wiki</a>.
Check out the code:
</p>

<pre>git clone https://github.com/haskell/hackage-server.git</pre>

<p>
We'd like to make it as simple as possible for anyone to <a href="https://github.com/haskell/hackage-server/wiki/Command-Line">set up a Hackage server</a>.
</p>
</div>
</body>
</html>
