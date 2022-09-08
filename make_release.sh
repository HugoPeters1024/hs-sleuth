#!/bin/sh
cd "${0%/*}"

mkdir release
mkdir release/static
echo "Building Server App"
cd ./server/
cabal build
cd ../
cp ./server/dist-newstyle/build/x86_64-linux/ghc-9.2.2/hs-comprehension-server-0.1.0.0/x/hs-comprehension-server/build/hs-comprehension-server/hs-comprehension-server release/server

echo "Building Elm Frontend"
cd ./frontend/
elm make --optimize src/Main.elm
cd ../
mv ./frontend/index.html release/static
cp ./frontend/src/style.css release/static
cp ./frontend/src/pygments.css release/static

echo "Zipping the result"
zip -r release.zip release



