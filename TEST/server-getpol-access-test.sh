echo 'get the policy'
curl -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run first four test cases for Policy (a)'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o2'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o2'
echo 'run first four test cases for Policy (b)'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o2'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o2'
echo end of curl tests
