# hup [![Hackage version](https://img.shields.io/hackage/v/hup.svg?label=Hackage)](https://hackage.haskell.org/package/hup)

[![Build Status](https://github.com/phlummox/hup/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/phlummox/hup/actions/workflows/ci.yml)
[![Windows build status](https://ci.appveyor.com/api/projects/status/htfimtle01wo328u/branch/master?svg=true&passingText=Windows%20build%20passing&failingText=Windows%20build%20failing&pendingText=Windows%20build%20pending)](https://ci.appveyor.com/project/phlummox/hup/branch/master)

Small program for building and uploading packages and documentation
built with the [`stack`][stack] build tool to a hackage server.

[stack]: https://www.haskellstack.org/

For instance, to build and upload package documentation to
<https://hackage.haskell.org/> for a package candidate:

```
$ hup docboth --candidate --user myHackageUserID --password myHackagePassword
```

Instead of providing a password on the command line, you can set the
`HUP_HACKAGE_PASSWORD` environment variable and `hup` will use that as the
password.

In addition to `stack`, `hup` requires the `cabal` executable,
but will install an appropriate `cabal` if it doesn't find one in the
binaries for the package snapshot your project is using.

## Installation

The recommended install method is to run:

```
$ stack --resolver=lts-11 build --copy-bins hup
```

On Linux, this will install `hup` to your `~/.local/bin` directory --
ensure that it's on your PATH, and you're good to go.

Alternatively, you can download a statically linked executable
(for Linux, AMD64 architecture) from the "[Releases][releases]"
page.

[releases]: https://github.com/phlummox/hup/releases

## Quick usage

Try something like:

```
$ cd /path/to/my/project
$ stack build
$ hup packboth -u myHackageUserID -p myHackagePassword
$ hup docboth -u myHackageUserID -p myHackagePassword
```

## Usage


* `hup [COMMAND] ... [OPTIONS]`

  Build and/or upload packages or documentation to a hackage server. A server
  url should be of the format PROTOCOL://SERVER[:PORT]/, and defaults to
  https://hackage.haskell.org/ if not specified.
  
  A password can also be given in the HUP_HACKAGE_PASSWORD environment variable
  instead of on the command line.
  
  'hup --help=all' will give help for all commands.

* Commands:

        packbuild  Build source distribution .tgz for a package.
        packup     Upload FILE as a package (or candidate package).
        packboth   Build source distribution .tgz and upload as package (or
                   candidate package).
        docbuild   Build documentation for a package.
        docup      Upload FILE as documentation.
        docboth    Build and upload documentation for a package.
      
* Common flags:

        -v --verbose          be verbose
        -h --help             Display help message
        -V --version          Print version information
           --numeric-version  Print just the version number
      

  '--help=bash' will output code for bash command-line completion.

### Subcommand details



* `hup packbuild [OPTIONS]`

  Build source distribution .tgz for a package.

* `hup packup [OPTIONS] FILE`

  Upload FILE as a package (or candidate package).

  Flags:

        -s --server=URL       
        -c --candidate        
        -u --user=USER        
        -p --password=PASSWORD
* `hup packboth [OPTIONS]`

  Build source distribution .tgz and upload as package (or candidate package).

  Flags:

        -s --server=URL       
        -c --candidate        
        -u --user=USER        
        -p --password=PASSWORD
* `hup docbuild [OPTIONS]`

  Build documentation for a package.

  Flags:

        -e --executables             Run haddock for Executables targets
        -t --tests                   Run haddock for Test Suite targets
        -i --internal                Run haddock for internal modules and include
                                     all symbols
           --haddock-arguments=ARGS  extra args to pass to haddock
        -q --quick                   quick build - don't build docco for
                                     dependencies (links may be broken)
* `hup docup [OPTIONS] FILE`

  Upload FILE as documentation.

  Flags:

        -s --server=URL       
        -c --candidate        
        -u --user=USER        
        -p --password=PASSWORD
* `hup docboth [OPTIONS]`

  Build and upload documentation for a package.

  Flags:

        -e --executables             Run haddock for Executables targets
        -t --tests                   Run haddock for Test Suite targets
        -i --internal                Run haddock for internal modules and include
                                     all symbols
           --haddock-arguments=ARGS  extra args to pass to haddock
        -q --quick                   quick build - don't build docco for
                                     dependencies (links may be broken)
        -s --server=URL            
        -c --candidate             
        -u --user=USER             
        -p --password=PASSWORD     


## Library API

For documentation of the library, see the [Hackage documentation][hackage-hup].

[hackage-hup]: https://hackage.haskell.org/package/hup

## Troubleshooting

### I get an error during upload that says "...: does not exist (no such protocol name: tcp)"

This is not actually a bug in `hup`, but is found in e.g. Docker containers
that don't have all the packages needed for networking - see e.g.
[here](https://stackoverflow.com/questions/46322773/yesod-app-in-docker-container-cant-make-network-requests) on StackOverflow.

You will need to install networking packages appropriate for your distro - on Ubuntu, something like ca-certificates, libgnutls28 (or another version of the GNU TLS library), and netbase.

### I get some sort of error when building documents that says "...haddock: internal error: ... hGetContents: invalid argument (invalid byte sequence)"

Again, this isn't actually a bug in `hup`, but happens (e.g. in Docker
containers) when the system locale is not properly set up (see a bug report
[here](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=871839) arising from a
similar issue). Annoyingly, `haddock` depends on the locale being properly set,
though it doesn't really seem necessary.

Try running `locale-gen "en_US.UTF-8"` to generate an appropriate UTF-8
locale, and `export LC_ALL="en_US.UTF-8"` so that the locale can be found
from environment variables.

## Bash command-line completion

To enable bash command-completion:

Run

```
$ hup --help=bash > hup.complete
```

then either "`source hup.complete`" or "`. hup.complete`".

## Defaults

Uses "`https://hackage.haskell.org/`" as the default server location,
but see the `DefaultServerUrl` module if you want to patch this to
something else before installing.

## Bugs and limitations

- Some very basic tests of library and app functionality are run on MS Windows
  and MacOS virtual machines using GitHub and Appveyor's CI/CD capabilities, but
  no extensive testing on those platforms is performed.

## Feature requests

So that actual bugs and defects aren't cluttered by other issues,
proposed features and feature requests are maintained as *closed* issues on
GitHub with the labels "enhancement" and "incomplete"
(see [here][feature-requests]).

[feature-requests]: https://github.com/phlummox/hup/issues?q=is%3Aissue+is%3Aclosed+label%3Aenhancement+label%3Aincomplete

## Credits

`hup` is a Haskellified version of [Oleg Grenrus's script][oleg],
which is a stack-enabled version of [Eric Mertens's script][eric].

[oleg]: http://web.archive.org/web/20210209123501/https://github.com/mstksg/binary-orphans/commit/3f106567260c1a9bb3063d49948201675876ad12.patch
[eric]: http://web.archive.org/web/20210209124009/https://github.com/ekmett/lens/commit/12b08783a3e44d46b41553d8a57560c6e68cf7e1.patch

<!--
  vim: syntax=markdown
-->
