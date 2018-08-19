#!/bin/bash
#

REV=unknown

which git > /dev/null 2>&1 && REV=`git log --format=%h -1`

echo -n $REV
