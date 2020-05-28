curl -s -G "http://127.0.0.1:8001/paapi/loadpol" --data-urlencode "policyfile=EXAMPLES/policy_signals_access.pl" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/loadpol" --data-urlencode "policyfile=EXAMPLES/policy_vehicle_ownership.pl" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/combinepol" --data-urlencode "policy1=Signals Access Policy" --data-urlencode "policy2=Vehicle Ownership Policy" --data-urlencode "combined=Combined Policy" --data-urlencode "token=admin_token"
echo 'set to Combined Policy'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Combined Policy" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'try to add and delete assign when neither the user or attribute are defined - should fail'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Combined Policy" --data-urlencode "policy_element=assign('User 1',uattr1)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Combined Policy" --data-urlencode "policy_element=assign('User 1',uattr1)" --data-urlencode "token=admin_token"
echo 'try to add and delete user - should succeed'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u3)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u3)" --data-urlencode "token=admin_token"
echo 'try to delete user when already deleted - should fail'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u3)" --data-urlencode "token=admin_token"
echo 'try a legitimate sequence of 12 add/delete operations'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u4)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u4)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
echo 'add assign should fail because user no longer exists'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
echo 'add the user back and then the add assign succeeds'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
echo 'get current policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'load a new policy'
curl -s -G "http://127.0.0.1:8001/paapi/loadpol" --data-urlencode "policyfile=EXAMPLES/policy3a.pl" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'set to the newly loaded policy'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Policy3" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'access checks, expect g d g g d'
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=jones" --data-urlencode "ar=read" --data-urlencode "object=mrec1"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=jones" --data-urlencode "ar=write" --data-urlencode "object=mrec1"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=smith" --data-urlencode "ar=write" --data-urlencode "object=mrec1"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=smith" --data-urlencode "ar=read" --data-urlencode "object=mrec1"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=smith" --data-urlencode "ar=read" --data-urlencode "object=Medical Records"
echo 'unload the policy'
curl -s -G "http://127.0.0.1:8001/paapi/purgepol" --data-urlencode "policy=Policy3" --data-urlencode "token=admin_token"
echo 'now what is the current policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'an access request with no current policy'
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=smith" --data-urlencode "ar=read" --data-urlencode "object=mrec1"
