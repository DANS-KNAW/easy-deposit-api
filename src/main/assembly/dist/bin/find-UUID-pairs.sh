#!/usr/bin/env bash

##################
# trouble shooting tool
#
# find draft-UUIDs together with ingest-UUIDs
#
# The result wil look like
# [<draft-UUID>] /easy-ingest-flow-inbox//<UUID>
#
# HINT: to find all submit attempts of a draft (as far as logging is still available)
# find-UUID-pairs.sh | grep '\[<draft-UUID-fragment>'

grep move /var/opt/dans.knaw.nl/log/easy-deposit-api/*.log | sed 's/.*INFO  //' | sed 's/move.*tmp//'