grep move /var/opt/dans.knaw.nl/log/easy-deposit-api/*.log \
 | sed 's!.*\[\(.*\)].* \(.*\)!echo;grep state.label /var/opt/dans.knaw.nl/tmp/easy-deposit-api/drafts/*/\1/deposit.properties \2/deposit.properties!' | sh