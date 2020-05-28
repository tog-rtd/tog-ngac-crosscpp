echo 'add/delete multiple'
echo 'first load ipolicy immediate'
curl -s -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(ipolicy,access,[
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
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo 'read the policy (explicitly named as ipolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo 'try to add and delete assign when neither the user or attribute are defined - should fail'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=ipolicy" --data-urlencode "policy_element=assign('User 1',uattr1)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=ipolicy" --data-urlencode "policy_element=assign('User 1',uattr1)" --data-urlencode "token=admin_token"

echo 'try to add and delete user - should succeed'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=ipolicy" --data-urlencode "policy_element=user(u3)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=ipolicy" --data-urlencode "policy_element=user(u3)" --data-urlencode "token=admin_token"

echo 'try to delete user when already deleted - should fail'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=ipolicy" --data-urlencode "policy_element=user(u3)" --data-urlencode "token=admin_token"

echo 'read the policy (explicitly named as ipolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo 'try a legitimate sequence of add/delete operations'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=ipolicy" --data-urlencode "policy_element=user(u4)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=ipolicy" --data-urlencode "policy_element=user(u4)" --data-urlencode "token=admin_token"

echo 'read the policy (explicitly named as ipolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"



echo 'try legitimate add/delete multiple operations'
curl -s -G "http://127.0.0.1:8001/paapi/deletem" --data-urlencode "policy=ipolicy" --data-urlencode "policy_elements=[assign(u1,ua1),user(u1)]" --data-urlencode "token=admin_token"

echo 'read the policy (two elements removed)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo 'add two elements back'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=ipolicy" --data-urlencode "policy_elements=[user(u1),assign('u1','ua1')]" --data-urlencode "token=admin_token"

echo 'read the policy (should be restored to original - except for order)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo end of add delete multiple tests
