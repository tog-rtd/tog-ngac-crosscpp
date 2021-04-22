echo 'set policy to "none"'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=none" --data-urlencode "token=admin_token"
echo 'get the policy'
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'load two policies'
curl -s -G "http://127.0.0.1:8001/paapi/loadpol" --data-urlencode "policyfile=EXAMPLES/policy_signals_access.pl" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/loadpol" --data-urlencode "policyfile=EXAMPLES/policy_vehicle_ownership.pl" --data-urlencode "token=admin_token"
echo 'form combined policy'
curl -s -G "http://127.0.0.1:8001/paapi/combinepol" --data-urlencode "policy1=Signals Access Policy" --data-urlencode "policy2=Vehicle Ownership Policy" --data-urlencode "combined=Combined Policy" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'try an access request without setting policy'
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Sebastian" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Door Signals"
echo 'set to Combined Policy'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Combined Policy" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/getpol" --data-urlencode "token=admin_token"
echo 'running nine test cases for Combined Policy, expecting d g g d d d d d d'
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Door Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Sebastian" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Door Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=r" --data-urlencode "object=VIN-3001 Shift Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Trip Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=r" --data-urlencode "object=VIN-3001 Trip Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=w" --data-urlencode "object=VIN-1001 Door Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=w" --data-urlencode "object=VIN-3001 Shift Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=w" --data-urlencode "object=VIN-1001 Trip Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=Ana" --data-urlencode "ar=w" --data-urlencode "object=VIN-3001 Trip Signals"
echo 'establish sessions'
curl -s -G "http://127.0.0.1:8001/paapi/initsession" --data-urlencode "user=Ana" --data-urlencode "session=ana_session" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/initsession" --data-urlencode "user=Sebastian" --data-urlencode "session=sebastian_session" --data-urlencode "token=admin_token"
echo 'repeat tests with sessions'
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Door Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=sebastian_session" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Door Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=r" --data-urlencode "object=VIN-3001 Shift Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Trip Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=r" --data-urlencode "object=VIN-3001 Trip Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=w" --data-urlencode "object=VIN-1001 Door Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=w" --data-urlencode "object=VIN-3001 Shift Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=w" --data-urlencode "object=VIN-1001 Trip Signals"
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=ana_session" --data-urlencode "ar=w" --data-urlencode "object=VIN-3001 Trip Signals"
echo 'end sessions'
curl -s -G "http://127.0.0.1:8001/paapi/endsession" --data-urlencode "session=ana_session" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/endsession" --data-urlencode "session=sebastian_session" --data-urlencode "token=admin_token"
echo 'try sebastian_session access now that session ended, expect deny'
curl -s -G "http://127.0.0.1:8001/pqapi/access" --data-urlencode "user=sebastian_session" --data-urlencode "ar=r" --data-urlencode "object=VIN-1001 Door Signals"
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=none" --data-urlencode "token=admin_token"
echo end of Combined Policy curl tests
