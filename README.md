# hup [![Hackage version](https://img.shields.io/hackage/v/hup.svg?label=Hackage)](https://hackage.haskell.org/package/hup) [![Linux Build Status](https://img.shields.io/travis/phlummox/hup.svg?label=Linux%20build)](https://travis-ci.org/phlummox/hup)

Small program for building and uploading packages and documentation
to a hackage server; a Haskellified version of
phadej's [script](https://github.com/phadej/binary-orphans/blob/master/hackage-docs.sh),
which is a stack-enabled version of ekmett's [script](https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh).

In addition to `stack`, requires that `cabal` and `haddock` are on your path.
(If you're using stack, they're easily installed with, e.g.  `stack install
cabal`.)

## Installation

Install in the standard Haskell way: `cabal install bogocopy`, or `stack
install bogocopy` if using Stack.

## Usage

* `hup [COMMAND] ... [OPTIONS]`
   
  Build and/or upload packages or documentation to a hackage server. A server
  url should be of the format `PROTOCOL://SERVER[:PORT]/`, and defaults to
  `http://localhost:8080/` if not specified.

  A password can also be given in the `PASSWORD` environment variable instead
  of on the command line.
  
  'hup ==help=all' will give help for all commands.

* Commands:

  - `packup`:    Upload FILE as a package (or candidate package).
  - `docbuild`:  Build documentation for a  package.
  - `docup`:     Upload FILE as documentation.
  - `docboth`:   Build and upload documentation for a package.


Common flags:
  -v ==verbose                               be verbose
  -h ==help                                  Display help message.
                                             '==help=all' will display help for
                                             all commnds. '==help=bash' will
                                             output code for bash command-line
                                             completion.
  -V ==version                               Print version information
     ==numeric-version                       Print just the version number

hup packup [OPTIONS] FILE
  Upload FILE as a package (or candidate package).

  -s ==server=URL                          
  -c ==candidate                           
  -u ==user=USER                           
  -p ==password=PASSWORD                   

hup docbuild [OPTIONS]
  Build documentation for a  package.

  -e ==executables                           Run haddock for Executables
                                             targets
  -t ==tests                                 Run haddock for Test Suite
                                             targets
     ==haddock-arguments=ARGS ==haddockargs  extra args to pass to haddock

hup docup [OPTIONS] FILE
  Upload FILE as documentation.

  -s ==server=URL                          
  -c ==candidate                           
  -u ==user=USER                           
  -p ==password=PASSWORD                   

hup docboth [OPTIONS]
  Build and upload documentation for a package.

  -e ==executables                           Run haddock for Executables
                                             targets
  -t ==tests                                 Run haddock for Test Suite
                                             targets
     ==haddock-arguments=ARGS ==haddockargs  extra args to pass to haddock
  -s ==server=URL                          
  -c ==candidate                           
  -u ==user=USER                           
  -p ==password=PASSWORD        

## Bash command-line completion


 to enable bash command-completion:

  run `hup ==help=bash > hup.complete`
  then `source hup.complete` or `. hup.complete`

-->

Uses "https://hackage.haskell.org/" as the default server location,
but see the "DefaultServerUrl" module if you want to patch this to
something else before installing.    

ADD VERBOSE FLAGS IN RIGHT BIT.
- cabal "-v2"


# limitations


# TODO


Copies a directory tree, preserving permissions and modification times, but
making zero-size sparse copies of big files.

## Installing and running

Install in the standard Haskell way: `cabal install bogocopy`, or `stack
install bogocopy` if using Stack.

Usage: 

        bogocopy [-v|--verbose] (-s|--size SIZE_MB) SRCDIR DSTDIR

> copy a directory tree, preserving permissions and modification times, but
> making zero-size sparse copies of big files
>
> `DSTDIR` will be created.

Available options:

`-h,--help`         

>  Show this help text

`--v,--verbose`     

>  Verbose (debugging) output

`--s,--size SIZE_MB`

>  Size limit, files leq to this size (in MB) are real-copied, those above are not.

### Bugs and limitations

- Limited to unix-like systems
- Uses the `rsync` and `cp` commands at run-time
- Won't preserve the "ctime" (inode change time) of a node
- Tested in only a desultory fashion, use at your own risk

