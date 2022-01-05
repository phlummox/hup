#!/usr/bin/env bash

# stop qemu VM or docker container

if [ "$#" -ne 2 ]; then
  printf >&2 'expected 2 args, a platform (one of "Linux", "Windows", "macOS"), and a container/pid\n'
  exit 1
fi

platform=$1
pid=$2

set -x
set -euo pipefail

case ${platform} in

  Linux|Windows|macOS)
    docker -D stop -t 1 "${pid}"
    ;;

  *)
    printf >&2 'Unrecognized OS'
    exit 1
    ;;
esac


