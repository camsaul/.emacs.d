#! /usr/bin/env bash

set -exuo pipefail

rm -rf eln-cache
rm -f *.elc
rm -f *.eld
rm -f lisp/*.elc
rm -f loaddefs.el
rm -f autoloads.el
