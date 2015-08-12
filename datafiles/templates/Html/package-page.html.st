<!DOCTYPE html>

  <head>
    $hackageCssTheme()$
    <title>
      $docTitle$
    </title>
    <!-- <link rel="canonical" href="http://localhost:8080/package/" $pkgName$ /> -->
  </head>

  <body>
    $hackagePageHeader()$

    <div id="content">

      <h1>The $pkgName$ package</h1>

      <div style="font-size: small">
        $headLinks$
      </div>

      $deprecatedMsg$
      <!-- Optional -->

      $pkgDescription$

      $skipToReadme$
      <!-- Optional -->

      <h2>Properties</h2>
      <table class="properties">
        <tbody>

          <tr>
            <th>Versions</th>
            <td>$versions$</td>
            </tr>

          <tr>
            <th>Change log</th>
            <td>$changelog$</td>
          </tr>

          <tr>
            <th>Dependencies</th>
            <td>$dependencies$</td>
          </tr>

          <tr>
            <th>License</th>
            <td>$license$</td>
          </tr>
          <tr>
            <th>Copyright</th>
            <td>$copyright$</td>
          </tr>
          <tr>
            <th>Author</th>
            <td>$author$</td>
          </tr>
          <tr>
            <th>Maintainer</th>
            <td>$maintainer$</td>
          </tr>
          <tr>
            <th>Category</th>
            <td>$categories$</td>
          </tr>
          <tr>
            <th>Home page</th>
            <td>$homePage$</td>
          </tr>
          <tr>
            <th>Bug tracker</th>
            <td>$bugTracker$</td>
          </tr>
          <tr>
            <th>Source repository</th>
            <td>$sourceRepo$</td>
          </tr>
          <tr>
            <th>Uploaded</th>
            <td>$uploadTime$</td>
          </tr>
          <tr>
            <th>Downloads</th>
            <td>0 total (0 in last 30 days)</td>
          </tr>
          <tr>
            <th>Votes</th>
            <td>$votes$</td>
          </tr>
          <tr>
            <th>Status</th>
            <td>Docs available <span style="font-size: small">[<a href=
                  "http://localhost:8080/package/Cabal-1.22.4.0/reports/1">build
                  log</a>]</span><br />
              Last success reported on 2015-06-26 <span style=
                "font-size: small">[<a href=
                  "http://localhost:8080/package/Cabal-1.22.4.0/reports/">all 1
                  reports</a>]</span></td>
          </tr>
        </tbody>
      </table>
      <h2>Modules</h2>
      <div id="module-list">
        <ul class="modules">
          <li><i>Distribution</i>
            <ul class="modules">
              <li><i>Compat</i>
                <ul class="modules">
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Compat-CreatePipe.html">
                        Distribution.Compat.CreatePipe</a></span></li>
                  <li><span class=
                      "module">Distribution.Compat.Environment</span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Compat-Exception.html">
                        Distribution.Compat.Exception</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Compat-ReadP.html">
                        Distribution.Compat.ReadP</a></span></li>
                </ul>
              </li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Compiler.html">
                    Distribution.Compiler</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-InstalledPackageInfo.html">
                    Distribution.InstalledPackageInfo</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-License.html">
                    Distribution.License</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Make.html">
                    Distribution.Make</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-ModuleName.html">
                    Distribution.ModuleName</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Package.html">
                    Distribution.Package</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-PackageDescription.html">
                    Distribution.PackageDescription</a></span>
                <ul class="modules">
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-PackageDescription-Check.html">
                        Distribution.PackageDescription.Check</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-PackageDescription-Configuration.html">
                        Distribution.PackageDescription.Configuration</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-PackageDescription-Parse.html">
                        Distribution.PackageDescription.Parse</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-PackageDescription-PrettyPrint.html">
                        Distribution.PackageDescription.PrettyPrint</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-PackageDescription-Utils.html">
                        Distribution.PackageDescription.Utils</a></span></li>
                </ul>
              </li>
              <li><span class="module">Distribution.ParseUtils</span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-ReadE.html">
                    Distribution.ReadE</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple.html">
                    Distribution.Simple</a></span>
                <ul class="modules">
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Bench.html">
                        Distribution.Simple.Bench</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Build.html">
                        Distribution.Simple.Build</a></span>
                    <ul class="modules">
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Build-Macros.html">
                            Distribution.Simple.Build.Macros</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Build-PathsModule.html">
                            Distribution.Simple.Build.PathsModule</a></span></li>
                    </ul>
                  </li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-BuildPaths.html">
                        Distribution.Simple.BuildPaths</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-BuildTarget.html">
                        Distribution.Simple.BuildTarget</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-CCompiler.html">
                        Distribution.Simple.CCompiler</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Command.html">
                        Distribution.Simple.Command</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Compiler.html">
                        Distribution.Simple.Compiler</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Configure.html">
                        Distribution.Simple.Configure</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-GHC.html">
                        Distribution.Simple.GHC</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-GHCJS.html">
                        Distribution.Simple.GHCJS</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Haddock.html">
                        Distribution.Simple.Haddock</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-HaskellSuite.html">
                        Distribution.Simple.HaskellSuite</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Hpc.html">
                        Distribution.Simple.Hpc</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Install.html">
                        Distribution.Simple.Install</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-InstallDirs.html">
                        Distribution.Simple.InstallDirs</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-JHC.html">
                        Distribution.Simple.JHC</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-LHC.html">
                        Distribution.Simple.LHC</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-LocalBuildInfo.html">
                        Distribution.Simple.LocalBuildInfo</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-PackageIndex.html">
                        Distribution.Simple.PackageIndex</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-PreProcess.html">
                        Distribution.Simple.PreProcess</a></span>
                    <ul class="modules">
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-PreProcess-Unlit.html">
                            Distribution.Simple.PreProcess.Unlit</a></span></li>
                    </ul>
                  </li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program.html">
                        Distribution.Simple.Program</a></span>
                    <ul class="modules">
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Ar.html">
                            Distribution.Simple.Program.Ar</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Builtin.html">
                            Distribution.Simple.Program.Builtin</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Db.html">
                            Distribution.Simple.Program.Db</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Find.html">
                            Distribution.Simple.Program.Find</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-GHC.html">
                            Distribution.Simple.Program.GHC</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-HcPkg.html">
                            Distribution.Simple.Program.HcPkg</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Hpc.html">
                            Distribution.Simple.Program.Hpc</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Ld.html">
                            Distribution.Simple.Program.Ld</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Run.html">
                            Distribution.Simple.Program.Run</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Script.html">
                            Distribution.Simple.Program.Script</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Strip.html">
                            Distribution.Simple.Program.Strip</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Program-Types.html">
                            Distribution.Simple.Program.Types</a></span></li>
                    </ul>
                  </li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Register.html">
                        Distribution.Simple.Register</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Setup.html">
                        Distribution.Simple.Setup</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-SrcDist.html">
                        Distribution.Simple.SrcDist</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Test.html">
                        Distribution.Simple.Test</a></span>
                    <ul class="modules">
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Test-ExeV10.html">
                            Distribution.Simple.Test.ExeV10</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Test-LibV09.html">
                            Distribution.Simple.Test.LibV09</a></span></li>
                      <li><span class="module"><a href=
                            "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Test-Log.html">
                            Distribution.Simple.Test.Log</a></span></li>
                    </ul>
                  </li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-UHC.html">
                        Distribution.Simple.UHC</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-UserHooks.html">
                        Distribution.Simple.UserHooks</a></span></li>
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Simple-Utils.html">
                        Distribution.Simple.Utils</a></span></li>
                </ul>
              </li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-System.html">
                    Distribution.System</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-TestSuite.html">
                    Distribution.TestSuite</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Text.html">
                    Distribution.Text</a></span></li>
              <li><i>Utils</i>
                <ul class="modules">
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Utils-NubList.html">
                        Distribution.Utils.NubList</a></span></li>
                </ul>
              </li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Verbosity.html">
                    Distribution.Verbosity</a></span></li>
              <li><span class="module"><a href=
                    "http://localhost:8080/package/Cabal-1.22.4.0/docs/Distribution-Version.html">
                    Distribution.Version</a></span></li>
            </ul>
          </li>
          <li><i>Language</i>
            <ul class="modules">
              <li><i>Haskell</i>
                <ul class="modules">
                  <li><span class="module"><a href=
                        "http://localhost:8080/package/Cabal-1.22.4.0/docs/Language-Haskell-Extension.html">
                        Language.Haskell.Extension</a></span></li>
                </ul>
              </li>
            </ul>
          </li>
        </ul>
      </div>
      <p style="font-size: small">[<a href=
          "http://localhost:8080/package/Cabal-1.22.4.0/docs/doc-index.html">Index</a>]</p>
      <h2>Flags</h2>
      <table class="flags-table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Description</th>
            <th>Default</th>
            <th>Type</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class="flag-name"><span class=
                "code">bundled-binary-generic</span></td>
            <td class="flag-desc"></td>
            <td class="flag-disabled">Disabled</td>
            <td class="flag-automatic">Automatic</td>
          </tr>
        </tbody>
      </table>
      <p class="tip"><span>Use</span> <span class="code">-f
          flag</span> <span>to enable a flag, or</span> <span class=
          "code">-f -flag</span> <span>to disable that flag.</span>
        <a href=
          "http://www.haskell.org/cabal/users-guide/installing-packages.html#controlling-flag-assignments">
          More info</a></p>
      <h2>Downloads</h2>
      <ul>
        <li><a href=
            "http://localhost:8080/package/Cabal-1.22.4.0/Cabal-1.22.4.0.tar.gz">
            Cabal-1.22.4.0.tar.gz</a> [<a href=
            "http://localhost:8080/package/Cabal-1.22.4.0/src/">browse</a>]
          (Cabal source package)</li>
        <li><a href=
            "http://localhost:8080/package/Cabal-1.22.4.0/Cabal.cabal">Package
            description</a> (included in the package)</li>
      </ul>
      <h4>Maintainers' corner</h4>
      <p>For package maintainers and hackage trustees</p>
      <ul>
        <li><a href="http://localhost:8080/package/Cabal/maintain">edit
            package information</a></li>
      </ul>
      <h2 id="readme">Readme for Cabal-1.22.4.0</h2>
      <div class="embedded-author-content">
        <h1>The Cabal library package</h1>
        <p>See the <a href="http://www.haskell.org/cabal/">Cabal web
            site</a> for more information.</p>
        <p>If you also want the <code>cabal</code> command-line program,
          you need the <a href=
            "http://localhost:8080/cabal-install/README.md">cabal-install</a>
          package in addition to this library.</p>
        <h1>Installing the Cabal library</h1>
        <h2>If you already have the <code>cabal</code> program</h2>
        <p>In this case run:</p>
<pre>

</pre>
        <p>However, if you do not have an existing version of the
          <code>cabal</code> program, you first must install the Cabal
          library. To avoid this bootstrapping problem, you can install the
          Cabal library directly as described below.</p>
        <h2>Installing as a user (no root or administrator access)</h2>
<pre>
<code>ghc -threaded --make Setup
./Setup configure --user
./Setup build
./Setup install
</code>
</pre>
        <p>Note the use of the <code>--user</code> flag at the configure
          step.</p>
        <p>Compiling 'Setup' rather than using <code>runghc Setup</code> is
          much faster and works on Windows. For all packages other than Cabal
          itself, it is fine to use <code>runghc</code>.</p>
        <p>This will install into <code>HOME/.cabal/</code> on Unix and
          into <code>Documents and Settings\User\Application
            Data\cabal\</code> on Windows. If you want to install elsewhere,
          use the <code>--prefix=</code> flag at the configure step.</p>
        <h2>Installing as root or Administrator</h2>
<pre>
<code>ghc -threaded --make Setup
./Setup configure
./Setup build
sudo ./Setup install
</code>
</pre>
        <p>Compiling Setup rather than using <code>runghc Setup</code> is
          much faster and works on Windows. For all packages other than Cabal
          itself, it is fine to use <code>runghc</code>.</p>
        <p>This will install into <code>/usr/local</code> on Unix, and on
          Windows it will install into <code>ProgramFiles/Haskell</code>. If
          you want to install elsewhere, use the <code>--prefix=</code> flag
          at the configure step.</p>
        <h1>Using older versions of GHC and Cabal</h1>
        <p>It is recommended that you leave any pre-existing version of
          Cabal installed. In particular, it is <em>essential</em> you keep
          the version that came with GHC itself, since other installed
          packages require it (for instance, the "ghc" API package).</p>
        <p>Prior to GHC 6.4.2, however, GHC did not deal particularly well
          with having multiple versions of packages installed at once. So if
          you are using GHC 6.4.1 or older and you have an older version of
          Cabal installed, you should probably remove it by running:</p>
<pre>
<code> ghc-pkg unregister Cabal
</code>
</pre>
          <p>or, if you had Cabal installed only for your user account,
            run:</p>
<pre>
<code> ghc-pkg unregister Cabal --user
</code>
</pre>
            <h1>The <code>filepath</code> dependency</h1>
            <p>Cabal uses the <a href=
                "http://hackage.haskell.org/package/filepath">filepath</a> package,
              so it must be installed first. GHC version 6.6.1 and later come
              with <code>filepath</code>, however, earlier versions do not by
              default. If you do not already have <code>filepath</code>, you need
              to install it. You can use any existing version of Cabal to do
              that. If you have neither Cabal nor <code>filepath</code>, it is
              slightly harder but still possible.</p>
            <p>Unpack Cabal and <code>filepath</code> into separate
              directories. For example:</p>
<pre>
<code>tar -xzf filepath-1.1.0.0.tar.gz
tar -xzf Cabal-1.6.0.0.tar.gz

# rename to make the following instructions simpler:
mv filepath-1.1.0.0/ filepath/
mv Cabal-1.6.0.0/ Cabal/

cd Cabal
ghc -i../filepath -cpp --make Setup.hs -o ../filepath/setup
cd ../filepath/
./setup configure --user
./setup build
./setup install
</code>
</pre>
              <p>This installs <code>filepath</code> so that you can install
                Cabal with the normal method.</p>
              <h1>More information</h1>
              <p>Please see the <a href="http://www.haskell.org/cabal/">Cabal web
                  site</a> for the <a href=
                  "http://www.haskell.org/cabal/users-guide">user guide</a> and
                <a href=
                  "http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html">
                  API documentation</a>. There is additional information available on
                the <a href="https://github.com/haskell/cabal/wiki">development
                  wiki</a>.</p>
              <h1>Bugs</h1>
              <p>Please report bugs and feature requests to Cabal's <a href=
                  "https://github.com/haskell/cabal/issues">bug tracker</a>.</p>
              <h2>Your help</h2>
              <p>To help Cabal's development, it is enormously helpful to know
                from Cabal's users what their most pressing problems are with Cabal
                and <a href="http://hackage.haskell.org/">Hackage</a>. You may have
                a favourite Cabal bug or limitation. Look at Cabal's <a href=
                  "https://github.com/haskell/cabal/issues">bug tracker</a>. Ensure
                that the problem is reported there and adequately described.
                Comment on the issue to report how much of a problem the bug is for
                you. Subscribe to the issues's notifications to discussed
                requirements and keep informed on progress. For feature requests,
                it is helpful if there is a description of how you would expect to
                interact with the new feature.</p>
              <h1>Source code</h1>
              <p>You can get the master development branch using:</p>
<pre>
<code> git clone https://github.com/haskell/cabal.git
</code>
</pre>
              <h1>Credits</h1>
              <p>Cabal developers (in alphabetical order):</p>
              <ul>
                <li>Krasimir Angelov</li>
                <li>Bjorn Bringert</li>
                <li>Duncan Coutts</li>
                <li>Isaac Jones</li>
                <li>David Himmelstrup ("Lemmih")</li>
                <li>Simon Marlow</li>
                <li>Ross Patterson</li>
                <li>Thomas Schilling</li>
                <li>Martin Sj&#195;&#182;gren</li>
                <li>Malcolm Wallace</li>
                <li>and nearly 30 other people have contributed occasional
                  patches</li>
              </ul>
              <p>Cabal specification authors:</p>
              <ul>
                <li>Isaac Jones</li>
                <li>Simon Marlow</li>
                <li>Ross Patterson</li>
                <li>Simon Peyton Jones</li>
                <li>Malcolm Wallace</li>
              </ul>
            </div>
          </div>

         $footer()$

        </body>
      </html>
