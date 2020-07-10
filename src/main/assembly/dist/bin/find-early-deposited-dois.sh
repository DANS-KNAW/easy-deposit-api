#!/usr/bin/env bash

##################
# trouble shooting tool
#
# find deposits of the eraliest days
# their bag-name is "bag", not the draft-UUID
#
# generate statements to find their DOI's

find /var/opt/dans.knaw.nl/tmp/easy-ingest-flow-inbox/ -user easy-deposit-api -print0 \
 | xargs -0 grep --exclude-dir bag -l ' = bag' 2>/dev/null \
 | sed "s/^/grep -H \'doi =\' /"