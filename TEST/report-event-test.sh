echo 'Report Event Test'

curl -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=test_event" --data-urlencode "token=epp_token"

curl -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=inval_event" --data-urlencode "token=epp_token"

curl -G "http://127.0.0.1:8001/epp/report_event" --data-urlencode "event=event(test_event,user(u1),policy_class(pc),operation(adnull),object(o1))" --data-urlencode "token=epp_token"

echo End of Report Event Test
