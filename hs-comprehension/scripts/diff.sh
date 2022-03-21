#!/usr/bin/env sh
git diff --no-index --color $1 $2 | ansi2html --partial
