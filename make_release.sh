#!/bin/sh
cd "${0%/*}"

mkdir release

echo "Building Elm Frontend"
cd ./frontend/
elm make --optimize src/Main.elm --output main.js
cd ../
mv ./frontend/main.js release
cp ./frontend/src/index_html_for_elm.html release/index.html
cp ./frontend/src/style.css release
cp ./frontend/src/pygments.css release
cp ./frontend/src/highlight.min.js release


