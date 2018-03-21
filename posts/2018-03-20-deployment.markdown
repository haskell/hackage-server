---
title: Latest Hackage Deployment
author: Herbert Valerio Riedel
description: As many of you may have noticed, there was a change in Hackage's appearance on Monday...
---

### Hackage Redesign

As many of you may have noticed, there was a change in [Hackage](https://hackage.haskell.org)'s appearance on Monday.

This is a first result of the design work being done by Nuno Alexandre
which is intended to give Haskell a <q>consistent, friendly and
welcoming look</q> as he explains in [his <q>Redesigning Haskell docs</q>
blogpost](https://nunoalexandre.com/2018/02/04/redesigning-haskell-docs). Note
that this is still ongoing work so the current state is not final
(most notably, the new Haddock stylesheets haven't been deployed yet);
and we require your feedback and suggestions to guide further
evolution!

If you want to provide feedback and suggestions, you can do so in
Hackage's [issue tracker](https://github.com/haskell/hackage-server/issues).

### Upgrade to Cabal 2.2

This deployment adds support
[Cabal 2.2](http://hackage.haskell.org/package/Cabal-2.2.0.0) which means
that effective immediately it's finally possible to upload packages
conforming to the <q>`cabal-version: 2.2`</q> cabal format version.

Noteworthy new features enabled by <q>`cabal-version: 2.2`</q> are:

 - [<q>`common`</q> Stanzas](http://cabal.readthedocs.io/en/latest/developing-packages.html?highlight=elif#common-stanzas) for reducing duplication in package specifications.
 - [New <q>`build-type:`</q> defaulting](http://cabal.readthedocs.io/en/latest/developing-packages.html?highlight=elif#pkg-field-build-type) which infers <q>`build-type:`</q> for <q>`Simple`</q>/<q>`Custom`</q> automatically.
 - [New <q>`elif`</q> keyword](http://cabal.readthedocs.io/en/latest/developing-packages.html?highlight=elif#conditional-blocks).
 - The <q>`license:`</q> field now supports [SPDX license identifiers](https://spdx.org/licenses/).
 
See [Cabal's changelog](http://hackage.haskell.org/package/Cabal-2.2.0.0/changelog) for a full list of changes.

### Uncurated Hackage Layer

This deployment also implements the first and second phases of the [<q>Uncurated Hackage Layer</q> proposal](https://github.com/gbaz/ecosystem-proposals/blob/gbaz-uncurated/proposals/0000-uncurated-layer.rst) by adding support for the new <q>`x-curation:`</q> field.
Quoting the relevant section of the [Hackage Upload Documentation](http://hackage.haskell.org/upload#versioning_and_curation):

<blockquote style="margin-left: 5em; margin-right: 5em" cite="http://hackage.haskell.org/upload#versioning_and_curation">
Package uploaders may choose to exclude individual package uploads
from curation, by setting the <q>`x-curation:`</q> field of the package's
`.cabal` file to <q>`uncurated`</q>. Packages which are uncurated have no
expectations on them regarding versioning policy. Trustees or
maintainers may adopt uncurated packages into the curated layer
through metadata revisions. Metadata revisions must not set the
value of the x-curation field to any variant of uncurated.
 
Two variants of the uncurated property are supported:
 
- First, <q>`uncurated-no-trustee-contact`</q>, which indicates that
  maintainers do not wish to be contacted by trustees regarding any
  metadata issues with the package. (Contact may still occur over
  issues that are not related to curation, such as licensing, etc.).
 
- Second, <q>`uncurated-seeking-adoption`</q>, which indicates that
  maintainers would like their package to be adopted in the curated
  layer, but currently some issue prevents this, which they would
  like assistance with.
</blockquote>

### Other Improvements

Other noteworthy improvemnts in this deployment include:

 - Serving of the [unfiltered package browse table](https://hackage.haskell.org/packages/browse) was [optimized](https://github.com/haskell/hackage-server/pull/699) resulting in a signficant speedup.

 - [Improve discoverability of user's own account management page](https://github.com/haskell/hackage-server/pull/689).

 - [Include package revision number in GUID of RSS feeds](https://github.com/haskell/hackage-server/issues/683).
