echo 'Event Triggered Response Test'

echo 'load policy immediate'
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
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'run test cases for ipolicy, expect: grant grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=r&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u1&ar=w&object=o1'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=r&object=o1'

echo report events
curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=test_event" --data-urlencode "token=epp_token"

curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=inval_event" --data-urlencode "token=epp_token"

curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=event(test_event,user(u1),policy_class(pc),operation(adnull),object(o1))" --data-urlencode "token=epp_token"


echo 'load erp immediate'
curl -s -G "http://127.0.0.1:8001/epp/loadi_erp" --data-urlencode "erp=er_package(er_test1,[
    er(
        ev_pat(user(any),policy_class(any),operation(adnull),object(any)),
        [addm([]),deletem([])]
    ),
    er(
        ev_pat(user(any),policy_class(any),operation(addone),object(any)),
        [add(ipolicy,user(u4))]
    ),
    er(
        ev_pat(user(any),policy_class(any),operation(addm1),object(any)),
        [
         addm(ipolicy,[
                  user(u2),
                  assign(u2,ua1)
                 ])
        ]),
    er(
        ev_pat(user(any),policy_class(any),operation(deletem1),object(any)),
        [
         deletem(ipolicy,[
		     user(u4),
                     assign(u2,ua1),
                     user(u2)
              ])
        ]) ])" --data-urlencode "token=epp_token"

echo 'read the policy (explicitly named as ipolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo report events that change the policy
curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=event(test_event,user(u1),policy_class(pc),operation(adnull),object(o1))" --data-urlencode "token=epp_token"

echo 'read the policy (should be unchanged)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo report event that changes the policy adding u4
curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=event(test_event,user(u1),policy_class(pc),operation(addone),object(o1))" --data-urlencode "token=epp_token"

echo 'read the policy (should be changed added u4)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo report event that changes the policy adding u2 and assign to ua1
curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=event(test_event,user(u1),policy_class(pc),operation(addm1),object(o1))" --data-urlencode "token=epp_token"

echo 'read the policy (should be changed added u2 and assign)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo report event that changes the policy deleting u4, u2 and assign to ua1
curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=event(test_event,user(u1),policy_class(pc),operation(deletem1),object(o1))" --data-urlencode "token=epp_token"

echo 'read the policy (should be changed deleted u4, u2 and assign)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"

echo report event that changes the policy AGAIN deleting u4, u2 and assign to ua1
curl -s -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=event(test_event,user(u1),policy_class(pc),operation(deletem1),object(o1))" --data-urlencode "token=epp_token"

echo 'read the policy (should be un-changed deletions ignored)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=ipolicy" --data-urlencode "token=admin_token"


echo End of Event Triggered Response Test
