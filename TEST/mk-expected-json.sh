#!/bin/sh
01-servercurltest.sh >01-expected-json
# diff 01-expected$1 01-result
02-serverCombinedtest.sh >02-expected-json
# diff 02-expected$1 02-result
03-loaditest2.sh >03-expected-json
# diff 03-expected$1 03-result
04-server-grantdeny-test.sh >04-expected-json
# diff 04-expected$1 04-result
05-add-delete-multiple-test.sh >05-expected-json
# diff 05-expected$1 05-result
06-server-new-feature-test.sh >06-expected-json
# diff 06-expected$1 06-result
07-context-change-test.sh >07-expected-json
# diff 07-expected$1 07-result
08-event-triggered-response-test.sh >08-expected-json
# diff 08-expected$1 08-result
09-cme-context-reg-test.sh >09-expected-json
# diff 09-expected$1 09-result
