echo 'Context Change Notification Test'
echo 'load immediate cpolicy'
curl -s -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(cpolicy,access,[
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
	cond( weekday, associate(ua1,[r,w],oa1) )
        ])" --data-urlencode "token=admin_token"

echo 'set to cpolicy'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=cpolicy" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'read the policy (explicitly named as cpolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=cpolicy" --data-urlencode "token=admin_token"

echo 'run test cases for cpolicy with current values, expect: grant grant deny if weekday==true else deny deny deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo 'run context_notify setting weekday to true'
curl -s --globoff "http://127.0.0.1:8001/epp/context_notify?context=[weekday:true]&token=epp_token"

echo 'run test cases for cpolicy, expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo 'run context_notify setting weekday to false'
curl -s --globoff "http://127.0.0.1:8001/epp/context_notify?context=[business:false,weekday:false]&token=epp_token"
echo 're-run test cases for cpolicy, expect: deny deny deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo 'run context_notify setting weekday to true'
curl -s -G "http://127.0.0.1:8001/epp/context_notify" --data-urlencode "context=[weekday:true]" --data-urlencode "token=epp_token"

echo 're-run test cases for cpolicy, expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo end of Context Change Notification Test
