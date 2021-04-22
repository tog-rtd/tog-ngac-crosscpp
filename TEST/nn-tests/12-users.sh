echo pqapi/users tests

echo 'set to Policy (a)'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Policy (a)" --data-urlencode "token=admin_token"

echo 'run users test cases for objects in Policy (a)'
echo expect 'o1=[(u1,[r,w]),(u2,[r])], o2=[(u1,[r]),(u2,[r,w])], o3=[(u2,[r,w])] o4=failure'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o2'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o3'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o4'

echo 'run users test cases for objects in Policy (b)'
echo expect 'o1=[(u1,[r,w]),(u2,[r])] o2=[(u1,[r]),(u2,[r,w])] o3=[(u2,[r,w])] o4=failure'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o2'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o3'
curl -s 'http://127.0.0.1:8001/pqapi/users?object=o4'

echo end of users tests
