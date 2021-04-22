echo 'set to Policy (a)'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Policy (a)" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run first four test cases for Policy (a), expect grant grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o2'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o2'
echo 'set to Policy (b)'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Policy (b)" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run first four test cases for Policy (b), expect deny deny grant grant'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o2'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o2'
echo end of curl tests
