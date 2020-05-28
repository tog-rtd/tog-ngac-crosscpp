#!/bin/sh
echo 01-servercurltest
01-servercurltest.sh >01-result-tmp
diff 01-expected$1 01-result-tmp

echo 02-serverCombinedtest
02-serverCombinedtest.sh >02-result-tmp
diff 02-expected$1 02-result-tmp

echo 03-loaditest2
03-loaditest2.sh >03-result-tmp
diff 03-expected$1 03-result-tmp

echo 04-server-grantdeny-test
04-server-grantdeny-test.sh >04-result-tmp
diff 04-expected$1 04-result-tmp

echo 05-delete-multiple-test
05-add-delete-multiple-test.sh >05-result-tmp
diff 05-expected$1 05-result-tmp

echo 06-server-new-feature-test
06-server-new-feature-test.sh >06-result-tmp
diff 06-expected$1 06-result-tmp

echo 07-context-change-test
07-context-change-test.sh >07-result-tmp
diff 07-expected$1 07-result-tmp

echo 08-event-triggered-response-test
08-event-triggered-response-test.sh >08-result-tmp
diff 08-expected$1 08-result-tmp

echo 09-cme-context-reg-test
09-cme-context-reg-test.sh 8002 >09-result-tmp
diff 09-expected$1 09-result-tmp

echo 10-servercurltest-accessm
10-servercurltest-accessm.sh 8002 >10-result-tmp
diff 10-expected$1 10-result-tmp
