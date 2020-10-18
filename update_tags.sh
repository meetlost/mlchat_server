#!/bin/sh

CWD=`pwd`

# Produce Project TAGS
find -L ${CWD} -name "*.[he]rl" | etags -o TAGS -
