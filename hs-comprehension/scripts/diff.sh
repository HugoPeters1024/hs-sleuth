#!/usr/bin/env sh
git diff --no-index --color --ignore-all-space --no-color-moved $1 $2 | ansi2html --partial
