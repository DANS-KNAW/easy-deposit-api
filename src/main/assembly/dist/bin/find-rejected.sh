#!/usr/bin/env bash

##################
# trouble shooting tool
#
# find all the deposits that were ever rejected
#
# generates statements to find the bag names of these deposits
# which should be the draft-UUID except for the deposits of the earliest days

find /var/opt/dans.knaw.nl/tmp/easy-ingest-flow-inbox/ -user easy-deposit-api -print0 \
 | xargs -0 grep --exclude-dir bag -l REJECTED 2>/dev/null \
 | sed 's/^/grep -H bag-name /'