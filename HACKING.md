# Hacking on Hup

## Re-generating the README

The README file is generated from hup's `--help` output.

Run `make README.md` to regenerate it. This requires [pweave][pweave],
which can be installed with: `pip3 install pweave`.

[pweave]: https://github.com/mpastell/Pweave

## Building with cabal

The hup executable is most easily built wwith `stack`, but
see the `cabal.project.freeze.*` files for working dependencies
for different platforms and versions of GHC.

## Docker image used for testing

See the [docker-hackage-server][server-repo] repository, which
which builds a Docker image that runs `hackage-server`.

[server-repo]: https://github.com/phlummox-dev/docker-hackage-server

