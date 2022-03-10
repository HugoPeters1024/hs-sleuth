#!/usr/bin/env python3
from pygments import highlight
from pygments.lexers import HaskellLexer
from pygments.formatters import HtmlFormatter

import sys

def main():
    code = "".join(sys.stdin.readlines())
    print(highlight(code, HaskellLexer(), HtmlFormatter()))

if __name__ == "__main__":
    main()

