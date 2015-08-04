#!/bin/bash

. private-aws-env.sh

case $* in
  "run")
    ./dist/build/amazonia/amazonia run-instance dev -k aws-key --ip 52.74.232.194
    ;;
  "kill")
    ./dist/build/amazonia/amazonia kill-instance dev
  ;;
esac

