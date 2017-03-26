<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Introduction | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>About next.hackage</h2>
<p>next.hackage is an experimental Hackage server for
upcoming features in Cabal that have not yet accepted by
Hackage.  Presently, it is a staging ground for
packages that use Backpack, an experimental new feature for package-level
modular development in GHC 8.2.</p>

<h3>Installing next.hackage</h3>

<p>In your <code>.cabal/config</code> file, add the following block:</p>

<pre>
remote-repo next-hackage
  url: http://next.hackage.haskell.org:8080/
  secure: True
  root-keys: 015ccb40c3cd254a7de458b048ba672c47e59195b3c8969cd85b686de2d49e2b
             0553aa6ab1e93a7c37d1d91670721b3b1ea8e976289c9709ebaaa9773df4538e
             f9cfba99add8cb35e94d16c1819903373ad45e9702557f0b4ec2cf10f2738216
  key-threshold: 2
</pre>

<h3>Uploading packages</h3>

<p>Email registration is not yet working, so please email <code>ezyang@mit.edu</code>
if you'd like to setup an account.</p>

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

