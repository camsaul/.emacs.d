#! /usr/bin/env bash

set -exuo pipefail

rm -rf eln-cache
rm -f *.elc lisp/*.elc
rm -f lisp/loaddefs.el
rm -f *.el~ lisp/*.el~
