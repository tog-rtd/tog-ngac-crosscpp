echo 'load policy immediate'
curl -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(ipolicy,access,[
	user(u1),
	user_attribute(ua1),
	object(o1),
	object_attribute(oa1),
	policy_class(access),
	connector('PM'),
	assign(u1,ua1),
	assign(o1,oa1),
	assign(ua1,access),
	assign(oa1,access),
	assign(access,'PM'),
	associate(ua1,[r,w],oa1)])" --data-urlencode "token=admin_token"

echo 'set to ipolicy'
curl -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run test cases for ipolicy, expect: grant grant deny'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo 'load modified policy immediate'
curl -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(ipolicy,access,[
        user(u1),
        user_attribute(ua1),
        object(o1),
        object_attribute(oa1),
        policy_class(access),
        connector('PM'),
        assign(u1,ua1),
        assign(o1,oa1),
        assign(ua1,access),
        assign(oa1,access),
        assign(access,'PM'),
        associate(ua1,[r],oa1)])" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run test cases for modified ipolicy, expect: grant deny deny'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo end of load immediate tests
