# hup [![Hackage version](https://img.shields.io/hackage/v/hup.svg?label=Hackage)](https://hackage.haskell.org/package/hup) [![Linux Build Status](https://img.shields.io/travis/phlummox/hup.svg?label=Linux%20build)](https://travis-ci.org/phlummox/hup) [![Windows Build Status](https://img.shields.io/appveyor/ci/phlummox/hup.svg?label=Windows%20build)](https://ci.appveyor.com/project/phlummox/hup)

Small program for building and uploading packages and documentation
built with `stack` to a hackage server; a Haskellified version of
[phadej's script](https://github.com/phadej/binary-orphans/blob/master/hackage-docs.sh),
which is a stack-enabled version of [ekmett's script](https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh).

In addition to `stack`, requires that `cabal` and `haddock` are on your path.
(If you're using stack, they're easily installed with, e.g.  `stack install
cabal-install`.)

## Installation

Install in the standard Haskell way: `cabal install hup`, or `stack
install hup`.

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


