echo 'Conditional Query Test'
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
	cond( is_True(_), associate(ua1,[r,w],oa1) )
        ])" --data-urlencode "token=admin_token"

echo 'set to cpolicy'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=cpolicy" --data-urlencode "token=admin_token"
#echo 'read the policy (explicitly named as cpolicy)'
#curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=cpolicy" --data-urlencode "token=admin_token"

echo 'run test cases for cpolicy without a condition, expect: deny deny deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo 're-run test cases for cpolicy with condition using caccess call, expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/caccess?user=u1&ar=r&object=o1&cond=is_True(true)'
curl -s 'http://127.0.0.1:8001/pqapi/caccess?user=u1&ar=w&object=o1&cond=is_True(true)'
curl -s 'http://127.0.0.1:8001/pqapi/caccess?user=u2&ar=r&object=o1&cond=is_True(true)'

echo 're-run test cases for cpolicy with condition added to ordinary access call, expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1&cond=is_True(true)'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1&cond=is_True(true)'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1&cond=is_True(true)'

echo '------------------------------'
echo 'load immediate cpolicy2'
curl -s -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(cpolicy2,access,[
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
cond( is_True(weekday), associate(ua1,[r,w],oa1) )
])" --data-urlencode "token=admin_token"

echo 'set to cpolicy2'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=cpolicy2" --data-urlencode "token=admin_token"
#echo 'read the policy (explicitly named as cpolicy2)'
#curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=cpolicy2" --data-urlencode "token=admin_token"

echo 'run test cases for cpolicy2 without a condition specified in the query'
echo 'if it is not a weekday expect: deny deny deny; if it is a weekday expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo 're-run test cases for cpolicy with condition using caccess call, expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/caccess?user=u1&ar=r&object=o1&cond=\[weekday=true\]'
curl -s 'http://127.0.0.1:8001/pqapi/caccess?user=u1&ar=w&object=o1&cond=\[weekday=true\]'
curl -s 'http://127.0.0.1:8001/pqapi/caccess?user=u2&ar=r&object=o1&cond=\[weekday=true\]'

echo 're-run test cases for cpolicy2 with condition added to ordinary access call, expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1&cond=\[weekday=true\]'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1&cond=\[weekday=true\]'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1&cond=\[weekday=true\]'

echo end of Conditional Query Test
