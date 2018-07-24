#!/bin/bash

case "$1" in
    -w|--watch)
    ./scripts/watch.sh;;

    -d|--deploy)
    ./scripts/deploy.sh;;

    *)
    ./scripts/build.sh;;
esac
