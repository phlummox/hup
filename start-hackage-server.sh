#!/usr/bin/env bash

# start hackage server, and print to stdout either a container ID
# or process ID to stop it with

# TODO:
# factor out hardcoded docker image, qcow file as script parameters

if [ "$#" -ne 1 ]; then
  printf >&2 'expected 1 arg, a platform (one of "Linux", "Windows", "macOS")\n'
  exit 1
fi

platform=$1

# Not used - all platforms now use docker
qemu_args="-netdev user,id=user.0,hostfwd=tcp::2233-:22,hostfwd=tcp::8080-:8080 \
	           -device virtio-net,netdev=user.0 \
	           -drive file=hackage_server_0.0.1.qcow2,if=virtio,cache=writeback,discard=ignore,format=qcow2 \
	           -machine type=pc \
	           -smp cpus=2,sockets=2 \
	           -m 400M \
	           -display none"

set -x
set -eo pipefail

case ${platform} in

  Linux|Windows|macOS)
    docker -D run --detach --rm -it -p 8080:8080 --hostname localhost --name hackage-server-ctr phlummox/hackage-server:0.1.0 \
      bash -c 'set -x; rm -f state/db/*/*/*.lock && rm -f state/db/*/*.lock && hackage-server run -v --ip=0.0.0.0 --static-dir=datafiles'
    ;;

  *)
    printf >&2 'Unrecognized OS'
    exit 1
    ;;
esac

