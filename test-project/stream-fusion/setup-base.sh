#!/bin/sh

# dump (and edit) the stream/list code into a copy of base-streams 
# such that the whole thing builds.

#
# assumes we're in the 'list' main repo
#

ghc_base_path=$*
if [ -z "$ghc_base_path" ] ; then
    echo "usage: ./setup-base.sh path_to_base_package"
    exit 1
fi

# Where are we sending things?
echo -n "Preparing to set up streams stuff under: "
echo $ghc_base_path

# Just double check its a base repo:
echo -n "Checking it looks like a proper base repo .... "
if [ -d "$ghc_base_path/_darcs/patches" ] ; then
    looks_ok=True
else
    looks_ok=False
fi
echo $looks_ok

if [ "$looks_ok" = "False" ] ; then
    echo "'$ghc_base_path' doesn't look like a darcs repo!"
    exit 1
fi

# Work out if we need to create the Data/List subdir
echo -n "Checking if we need to create the Data/List subdir... "
ghc_base_streams="$ghc_base_path/Data/List"
if [ ! -d "$ghc_base_streams" ] ; then
    create_streams=True
else
    create_streams=False
fi
echo $create_streams

if [ "$create_streams" = "True" ] ; then
    mkdir $ghc_base_streams
fi

# copy first
echo "{-# OPTIONS_GHC -fno-implicit-prelude #-}" > $ghc_base_streams/Stream.hs
echo "{-# OPTIONS_GHC -fno-implicit-prelude #-}" > $ghc_base_path/Data/Stream.hs

cat Data/List/Stream.hs >> $ghc_base_streams/Stream.hs
cat Data/Stream.hs      >> $ghc_base_path/Data/Stream.hs


echo "done."
