echo 'set policy to deny'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=deny" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run first four test cases'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o2'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o2'
echo 'set policy to grant'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=grant" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run first four test cases'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o2'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o2'
echo end of curl tests
