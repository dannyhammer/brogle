#!/bin/bash

count=$(ls ./releases/ | wc -l)
name="engine-v$count"

if [ $@ > 1 ]; then
    name="$name:$1" # Tag it, if a tag was supplied
fi;

echo "Building $name"

cargo b --release
cp target/release/brogle_engine releases/$name

echo "Built releases/$name"