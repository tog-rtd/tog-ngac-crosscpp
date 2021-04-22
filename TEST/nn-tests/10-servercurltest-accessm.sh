echo accessm tests
echo 'set to Policy (a)'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Policy (a)" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run first four test cases for Policy (a), expect grant grant grant deny'
curl -s -G "http://127.0.0.1:8001/pqapi/accessm" --data-urlencode "access_queries=[(u1,r,o1),(u1,w,o1),(u2,r,o2),(u1,w,o2)]"
echo 'set to Policy (b)'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Policy (b)" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run first four test cases for Policy (b), expect deny deny grant grant'
curl -s -G "http://127.0.0.1:8001/pqapi/accessm" --data-urlencode "access_queries=[(u1,r,o1),(u1,w,o1),(u1,r,o2),(u1,w,o2)]"
echo end of accessm tests
