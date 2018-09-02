#!/bin/bash

case "$1" in
    -w|--watch)
    ./scripts/watch.sh;;

    -d|--deploy)
    ./scripts/deploy.sh;;

    --debug)
    ./scripts/debug.sh;;

    *)
    ./scripts/build.sh;;
esac
