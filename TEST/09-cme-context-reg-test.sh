#!/bin/sh
# first argument is port number of the CME, second argument is a list of context variable names, e.g. [weekday,business]

if [ "$1" == "" ]
then
  echo need a port number as argument
  exit
else
  echo 'Context Change Registration Test on port' $1
fi
if [ "$2" == "" ]
then
	cvars='[weekday,business,nonexistent]'
else
	cvars="$2"
fi
echo registering $cvars

curl -s --globoff "http://127.0.0.1:$1/cross-cpp/context_notification_registration?context_variables=$cvars&epp_url=http://localhost:8001/epp/context_notify&epp_token=epp_token"

# curl -s --globoff -G
# "http://127.0.0.1:8002/cross-cpp/context_notification_registration"
# --data-urlencode "context_variables=[weekday,business]"
# --data-urlencode "epp_token=epp_token" --data-urlencode
# "epp_url=http://localhost:8001/epp/context_notify"

echo end of Context Change Registration Test
