# hup [![Hackage version](https://img.shields.io/hackage/v/hup.svg?label=Hackage)](https://hackage.haskell.org/package/hup)


| Build status |
| --- |
| [![Linux Build Status](https://badges.herokuapp.com/travis.org/phlummox/hup?branch=master&env=OS%3Dlinux&label=Linux%20build)](https://travis-ci.org/phlummox/hup) |
| [![OSX Build Status](https://badges.herokuapp.com/travis.org/phlummox/hup?branch=master&env=OS%3Dosx&label=OSX%20build)](https://travis-ci.org/phlummox/hup) |
| [![Windows Build Status](https://img.shields.io/appveyor/ci/phlummox/hup.svg?label=Windows%20build)](https://ci.appveyor.com/project/phlummox/hup) |

Small program for building and uploading packages and documentation
built with `stack` to a hackage server; a Haskellified version of
[phadej's script](https://github.com/phadej/binary-orphans/blob/master/hackage-docs.sh),
which is a stack-enabled version of [ekmett's script](https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh).

In addition to `stack`, it requires the `cabal` executable
(but will install an appropriate `cabal` if it doesn't find one in the
binaries for the package snapshot your project is using).

## Installation

Install in the standard Stack way with `stack install hup`.

Or, if you're using Linux on `x86_64` architecture, you can
download binaries from the GitHub repository, under "Releases".

## Quick usage

Try:

~~~
$ cd /path/to/my/project
$ stack build
$ hup packboth -u myHackageUserID -p myHackagePassword
$ hup docboth -u myHackageUserID -p myHackagePassword
~~~

## Usage

* `hup [COMMAND] ... [OPTIONS]`

  Build and/or upload packages or documentation to a hackage server. A server
  url should be of the format `PROTOCOL://SERVER[:PORT]/`, and defaults to
  `https://hackage.haskell.org/` if not specified.

  A password can also be given in the `PASSWORD` environment variable instead
  of on the command line.

  'hup --help=all' will give help for all commands.

* Commands:

        packup    Upload FILE as a package (or candidate package).
        docbuild  Build documentation for a  package.
        docup     Upload FILE as documentation.
        docboth   Build and upload documentation for a package.


* Common flags:

        -v --verbose          be verbose
        -h --help             Display help message. '--help=all' will display help
                              for all commnds. '--help=bash' will output code for
                              bash command-line completion.
        -V --version          Print version information
           --numeric-version  Print just the version number


* `hup packup [OPTIONS] FILE`
  Upload FILE as a package (or candidate package).

  Flags:

        -s --server=URL
        -c --candidate
        -u --user=USER
        -p --password=PASSWORD


* `hup docbuild [OPTIONS]`
  Build documentation for a  package.

  Flags:

        -e --executables             Run haddock for Executables targets
        -t --tests                   Run haddock for Test Suite targets
        -i --internal                Run haddock for internal modules and include
                                     all symbols
           --haddock-arguments=ARGS  extra args to pass to haddock
        -q --quick                   quick build - don't build docco for
                                     dependencies (links will be broken)

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

    hup --help=bash > hup.complete

then either "`source hup.complete`" or "`. hup.complete`".

## Defaults

Uses "`https://hackage.haskell.org/`" as the default server location,
but see the `DefaultServerUrl` module if you want to patch this to
something else before installing.


## Bugs and limitations

- Not yet tested on MS Windows or MacOS


