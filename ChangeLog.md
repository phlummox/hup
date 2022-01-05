# Changelog for Hup

Major changes to the library API and executable behaviour will be
documented here.

## Unreleased

- Library API: no changes.
- Executable behaviour: the environment variable used to pass in a password
  has changed from PASSWORD to HUP_HACKAGE_PASSWORD.
  (Fixes [#14](https://github.com/phlummox/hup/issues/14))
- Repository changes:
  - Renamed source code directories
  - Did some (API-unchanging) tidying of source code files
  - Got basic builds and tests working on Windows and MacOS again
    (fixes [#7](https://github.com/phlummox/hup/issues/7))
  - Added a "smoke test" (test of basic app functionality)
    against an instance of hackage-server running in a Docker
    container.

## 0.3.0.2

- Library API: no changes.
- Executable behaviour: no change.
- Changes to code and cabal file:
  - Tightened bounds on dependencies, to avoid known
    incompatible versions of dependencies.
  - Internal tidying of unused imports, trailing whitespace,
    missing sigs etc.
  - Enabled extra ghc warning flags during build.
  - Shifted "stack.yaml" out of top directory - poor form :/
  - Added some extra compilation flags (e.g.
    for tests that are unlikely to run without "stack",
    like doctests).

## 0.3.0.1

- Library API: no changes.
- Executable behaviour: no change.
- Some changes to README and Travis config file, and
  thus to the deployed "releases" binary, though.

## 0.3.0.0

- Bug fixes
- Updated documentation, added a "quick usage" section and troubleshooting
- `hup` now checks whether `cabal` is in the package snapshot binaries
  directory, and installs it if not.
- Added `packbuild` and `packboth` commands.
- Added a program test to .travis.yml

## 0.2.0.0

- Bug fixes
- Allow an Upload to store contents of file to be uploaded.
- Support older versions of directory and http-client
- Allow extra args to be passed to cabal when configuring or when
  running haddock.
- Added hspec and docstring tests
- Now running travis CI on a range of platforms and ghc versions.


