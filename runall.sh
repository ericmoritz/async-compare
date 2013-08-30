#!/usr/bin/env bash
COUNT=1

for i in $(seq 1 $COUNT); do
    for name in $@; do
	pushd $name >/dev/null
	echo -n "$name	"
	./firsts-async 1 news videos gifs funny
	popd >/dev/null
    done
done

