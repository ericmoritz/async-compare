#!/usr/bin/env bash
COUNT=10

for i in $@; do
    pushd $i >/dev/null
    echo -$i
    ./firsts-async $COUNT news videos gifs funny
    popd >/dev/null
done
