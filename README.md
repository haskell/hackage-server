# hackage-server

[![Build Status](https://travis-ci.org/haskell/hackage-server.png?branch=master)](https://travis-ci.org/haskell/hackage-server)

This is the `hackage-server` code. This is what powers <http://hackage.haskell.org>, and many other private hackage instances. The `master` branch is suitable for general usage. Specific policy and documentation for the central hackage instance exists in the `central-server` branch.

## Installing ICU

ICU stands for "International Components for Unicode". The `icu4c` is a set
of libraries that provide Unicode and Globalization support.
The [text-icu](https://hackage.haskell.org/package/text-icu) Haskell package
uses the [icu4c](http://icu-project.org/apiref/icu4c/) library to build.

You'll need to do the following to get hackage-server's dependency `text-icu` to build:

### Mac OS X

    brew install icu4c
    brew link icu4c --force

### Ubuntu/Debian

    sudo apt-get update
    sudo apt-get install unzip libicu-dev

### Fedora/CentOS
    sudo dnf install unzip libicu-devel

## Setting up security infrastructure

Out of the box the server comes with some example keys and TUF metadata. The
example keys are in `example-keys/`; these keys were used to create

    datafiles/TUF/root.json
    datafiles/TUF/mirrors.json
    datafiles/TUF/timestamp.private
    datafiles/TUF/snapshot.private

While these files will enable you to start the server without doing anything
else, you should replace all these files before deploying your server. In the
remainder of this section we will explain how to do that.

The first step is to create your own keys using the
[hackage-repo-tool](http://hackage.haskell.org/package/hackage-repo-tool):

    hackage-repo-tool create-keys --keys /path/to/keys

Then copy over the timestamp and snapshot keys to the TUF directory:

    cp /path/to/keys/timestamp/<id>.private datafiles/TUF/timestamp.private
    cp /path/to/keys/snapshot/<id>.private  datafiles/TUF/snapshot.private

Create root information:

    hackage-repo-tool create-root --keys /path/to/keys -o datafiles/TUF/root.json

And finally create a list of mirrors (this is necessary even if you don't have
any mirrors):

    hackage-repo-tool create-mirrors --keys /path/to/keys -o datafiles/TUF/mirrors.json

The `create-mirrors` command takes a list of mirrors as additional arguments if
you do want to list mirrors.

In order for secure clients to bootstrap the root security metadata from your
server, you will need to provide them with the public key IDs of your root keys;
you can find these as the file names of the files created in
`/path/to/keys/root` (as well as in the generated root.json under the
`signed.roles.root.keyids`). An example `cabal` client configuration might look
something like

    repository my-private-hackage
      url: http://example.com:8080/
      secure: True
      root-keys: 865cc6ce84231ccc990885b1addc92646b7377dd8bb920bdfe3be4d20c707796
                 dd86074061a8a6570348e489aae306b997ed3ccdf87d567260c4568f8ac2cbee
                 e4182227adac4f3d0f60c9e9392d720e07a8586e6f271ddcc1697e1eeab73390
      key-threshold: 2

## Running

    cabal install -j --enable-tests

    hackage-server init
    hackage-server run

If you want to run the server directly from the build tree, run

    dist/build/hackage-server/hackage-server run --static-dir=datafiles/

By default the server runs on port `8080` with the following settings:

    URL:      http://localhost:8080/
    username: admin
    password: admin

To specify something different, see `hackage-server init --help` for details.

The server can be stopped by using `Control-C`.

This will save the current state and shutdown cleanly. Running again
will resume with the same state.

### Resetting

To reset everything, kill the server and delete the server state:

```bash
rm -rf state/
```

Note that the `datafiles/` and `state/` directories differ:
`datafiles` is for static html, templates and other files.
The `state` directory holds the database (using `acid-state`
and a separate blob store).

### Creating users & uploading packages

* Admin front-end: <http://localhost:8080/admin>
* List of users: <http://localhost:8080/users/>
* Register new users: <http://localhost:8080/users/register>

Currently there is no restriction on registering, but only an admin
user can grant privileges to registered users e.g. by adding them to
other groups. In particular there are groups:

 * admins `http://localhost:8080/users/admins/` -- administrators can
   do things with user accounts like disabling, deleting, changing
   other groups etc.
 * trustees `http://localhost:8080/packages/trustees/` -- trustees can
   do janitorial work on all packages
 * mirrors `http://localhost:8080/packages/mirrorers/` -- for special
   mirroring clients that are trusted to upload packages
 * per-package maintainer groups
   `http://localhost:8080/package/foo/maintainers` -- users allowed to
   upload packages
 * uploaders `http://localhost:8080/packages/uploaders/` -- for
   uploading new packages

### Mirroring

There is a client program included in the hackage-server package called
hackage-mirror. It's intended to run against two servers, syncing all the
packages from one to the other, e.g. getting all the packages from the old
hackage and uploading them to a local instance of a hackage-server.

To try it out:

1. On the target server, add a user to the mirrorers group via
   http://localhost:8080/packages/mirrorers/
1. Create a config file that contains the source and target
   servers. Assuming you are cloning the packages on
   <http://hackage.haskell.org> locally, create the file servers.cfg:
```
source "hackage"
  uri: http://hackage.haskell.org
  type: secure

target "mirror"
  uri: http://admin:admin@localhost:8080
  type: hackage2

  post-mirror-hook: "shell command to execute"
```
Recognized types are hackage2, secure and local. The target server name was displayed when you ran.

Note, the target must _not_ have a trailing slash, or confusion will tend to occur. Additionally, if you have ipv6 setup on the machine, you may need to replace `localhost` with `127.0.0.1`.

Also note that you should mirror _from_ hackage2 or secure typically and mirror _to_ hackage2. Only mirroring from secure will include dependency revision information.

```bash
   hackage-server run.
```

1. Run the client, pointing to the config file:

```bash
hackage-mirror servers.cfg
```

This will do a one-time sync, and will bail out at the first sign of
trouble. You can also do more robust and continuous mirroring. Use the
flag `--continuous`. It will sync every 30 minutes (configurable with
`--interval`). In this mode it carries on even when some packages
cannot be mirrored for some reason and remembers them so it doesn't
try them again and again. You can force it to try again by deleting
the state files it mentions.
