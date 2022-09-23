#!/bin/sh
cd "${0%/*}"

mkdir release

echo "Building Elm Frontend"
cd ./frontend/
elm make --optimize src/Main.elm --output main.js
cd ../

mv ./frontend/main.js release
uglifyjs release/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output release/main.js && uglifyjs release/main.js --mangle --output release/main.js

cp ./frontend/src/index_html_for_elm.html release/index.html
cp ./frontend/src/style.css release
cp ./frontend/src/pygments.css release
cp ./frontend/src/highlight.min.js release

