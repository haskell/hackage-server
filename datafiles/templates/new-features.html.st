<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>New features in Hackage 2</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">

<h2>New features in Hackage 2</h2>
<p>Though our main priority has been feature parity so that we can switch over, volunteers have contributed several new features:</p
><ul
><li
  ><p
    >Package search: you can search by keywords that appear in the package name, synopsis or description. We expect this will be extend later to include tags, module names, author names, and metric-based ranking.</p
    ></li
  ><li
  ><p
    >A new visual theme for the site, using the CSS borrowed from haddock.</p
    ></li
  ><li
  ><p
    >Improved security: per-package maintainer groups. Only those users may upload new versions of the package. The server also now uses HTTP digest authentication rather than basic authentication.</p
    ></li
  ><li
  ><p
    >Adjusting package dependencies after a release to match reality: some package metadata can be edited after a package tarball is uploaded. In particular the dependency version constraints can be tightened or relaxed. The original tarball is not altered.</p
    ></li
  ><li
  ><p
    >Hackage “trustees”: a privileged group of users who help to curate the collection of packages as a whole (or some subset). They can adjust package tags and metadata — including fixing dependencies. The hope is that this will significantly improve the number of packages that build “out of the box”.</p
    ></li
  ><li
  ><p
    >Package changelogs, when they are included at top level in the package tarball.</p
    ></li
  ><li
  ><p
    >The contents of package tarballs can be browsed online.</p
    ></li
  ><li
  ><p
    >User self-registration via email, and self-service password reset. The current policy however is that before a new user can upload packages they must be added to the “uploaders” group by an administrator.</p
    ></li
  ><li
  ><p
    >Documentation can be uploaded by any suitably-authorised user, not just a single dedicated documentation build bot. In particular maintainers can upload documentation bundles if the doc build bots fail to do so.</p
    ></li
  ><li
  ><p
    >A RESTful API for getting and updating most of the data that the server holds, plus an auto-generated <a href="/api"
      >API description</a
      > / sitemap.</p
    ></li
  ></ul
><p
>There are also a number of new features that volunteers have partially implemented or that are in need of improvement. These are being tracked <a href="https://github.com/haskell/hackage-server/issues">on GitHub</a>.</p
><ul
><li
  ><p
    >Tags: packages can have arbitrary sets of tags, initially set from the categories in the .cabal file. This should be extended so that we can consolidate tags that should be aliases and tags should be used in the package search. See <a href="https://github.com/haskell/hackage-server/issues/24"
      >issue #24</a
      > and <a href="https://github.com/haskell/hackage-server/issues/27"
      >issue #27</a
      >.</p
    ></li
  ><li
  ><p
    >Reverse dependencies. This feature has been implemented but is currently disabled because it used to much memory. This should be investigated, the data structured adjusted and the feature re-enabled. The number of reverse dependencies should be an important component of a package popularity/quality metric. See <a href="https://github.com/haskell/hackage-server/issues/40"
      >issue #40</a
      >.</p
    ></li
  ><li
  ><p
    >Package “candidates”. You may have noticed that package versions are often uploaded in quick succession — sometimes just minutes apart — because a mistake is only noticed after the author uploaded. This feature lets you upload a “candidate”, giving it a URL that others can download from, and gives an opportunity for build bots and documentation builders to try the package out. Once the author is satisfied then they can publish to the main package index. We think this feature is about 90% complete. See <a href="https://github.com/haskell/hackage-server/issues/41"
      >issue #41</a
      >.</p
    ></li
  ><li
  ><p
    >Build reporting. There is support for build bots to upload build reports and build logs. This needs to be extended to include the anonymous build reports that cabal-install can generate. This way we can gather huge amounts of data. The goal is to inform users and maintainers about what packages work in what circumstances by digesting this data into useful information. In particular maintainers and trustees can then edit package dependencies to match reality. See <a href="https://github.com/haskell/hackage-server/issues/44"
      >issue #44</a
      >.</p
    ></li
  ><li
  ><p
    >All resources in machine readable formats. Many resources have JSON or other machine readable formats, but not yet all. See the <a href="/api"
      >api page</a
      >. This is an easy way to contribute to the development. See <a href="https://github.com/haskell/hackage-server/issues/42"
      >issue #42</a
      >.</p
    ></li
  ><li
  ><p
    >Site visual and information design. While we have at least switched to use the haddock “Ocean” theme, there’s a lot more that could be done by someone good at web design. Currently some pages use templates and other pages are generated in code. We should move towards consistently using templates to make it easier to adjust the site. It would also be possible to make more use of client-side technologies, rather than just classic html4 forms. See <a href="https://github.com/haskell/hackage-server/issues/43"
      >issue #43</a
      >.</p
    ></li
  ></ul
>

</div> <!-- content -->
</body>
</html>
