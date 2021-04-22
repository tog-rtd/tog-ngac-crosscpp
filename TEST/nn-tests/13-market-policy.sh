#!/bin/sh
# this script runs the marketplace example
# see EXAMPLES/Marketplace_example.pptx for description of the scenario
#

echo 'demo marketplace policy'

echo 'reset conditions before tests are run'
curl -s -G "http://127.0.0.1:8001/paapi/resetcond" --data-urlencode "token=admin_token"

echo 'first load immediate the core market_policy mpolicy'
# Stage 1 -- root of the policy - add service provider and user/dev could be done separately
# template: loadi( connector('PM'), policy_class(POLICY_CLASS), assign(POLICY_CLASS,'PM) ... )
#
# add a service provider
# template: user_attribute(SERVICE_PROVIDER_ID), assign(SERVICE_PROVIDER_ID, POLICY_ID)
#
# Stage 3 -- register a device owner and their device
# template: object_attribute(OWNER_ID), assign(OWNER_ID, POLICY_CLASS),
#           object(DEVICE_ID), assign(DEVICE_ID, OWNER_ID)
#
curl -s -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(mpolicy,market_policy,[
	connector('PM'),
	policy_class(market_policy),
	assign(market_policy,'PM'),
	user_attribute(sp_5f1aa9f638189e22005d0f39),
	assign(sp_5f1aa9f638189e22005d0f39, market_policy),
	object_attribute(owner_1),
	assign(owner_1, market_policy),
	object('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4'),
	assign('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4', owner_1)
	])" --data-urlencode "token=admin_token"

echo 'set current policy to mpolicy'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=mpolicy" --data-urlencode "token=admin_token"

# read-out the policy so far
echo 'read the policy (explicitly named as mpolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=mpolicy" --data-urlencode "token=admin_token"

echo 'install an offer: condition variables, condition predicate and aeon channel as user'
# Stage 2 -- add conditions for an offer (see below the corresponding aeon channel for the offer)
# template: loadcondi( condition_variable(NAME:TYPE), ..., condition_predicate(COND_PRED_DECLARATION), (COND_PRED_DEFINITION) )
#
curl -s -G "http://127.0.0.1:8001/paapi/loadcondi" --data-urlencode "cond_elements=[
	condition_variable(devid:name), condition_variable(mchan:number),
	condition_variable(tstart:timetext), condition_variable(tstop:timetext),
	condition_variable(tsubmit:timetext), condition_variable(loMin:number),
	condition_variable(loMax:number), condition_variable(laMin:number), condition_variable(laMax:number),
	condition_predicate( dr_offer_5f5a39f2b559dcf200f424d0, [name,number,timetext,timetext,timetext,number,number,number,number] ),
	(dr_offer_5f5a39f2b559dcf200f424d0(Dev,Chan,Start,Stop,Submit,LoMin,LoMax,LaMin,LaMax) :-
		Dev == '95b40cf9-a9fc-4bd8-b695-99773b6f25e4', channel_in_channels( Chan, [1, 2] ),
		timetextrange_in_range( Start, Stop, '2019-09-10T14:36:34.682Z', '2020-09-10T14:36:34.682Z' ),
		timetext_in_range( Submit, '2019-09-10T14:36:34.682Z', '2020-09-10T14:36:34.682Z' ),
		gbox_in_gbox( LoMin, LoMax, LaMin, LaMax, -9.39, 4.3, 35.95, 43.75) )
	]" --data-urlencode "token=admin_token"

# read out the conditions
echo 'read the conditions'
curl -s -G "http://127.0.0.1:8001/paapi/readcond" --data-urlencode "token=admin_token"

echo 'add the aeon channel for an offer'
# Stage 2 -- add an aeon channel for an offer (see above the addition of the conditions for the offer)
# template: addm( user(AEON_CHANNEL_ID), assign(AEON_CHANNEL_ID, SERVICE_PROVIDER_ID) )
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=mpolicy" --data-urlencode "policy_elements=[
	user(achnl_5f5a39f20463e50012bca2c3),
	assign(achnl_5f5a39f20463e50012bca2c3, sp_5f1aa9f638189e22005d0f39)
    ]" --data-urlencode "token=admin_token"

# read-out the policy so far
echo 'read the policy (explicitly named as mpolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=mpolicy" --data-urlencode "token=admin_token"

# add a contract
echo 'now add a contract to the policy'
# Stage 4 -- add a contract
# template: addm( [ user_attribute(UATTR_CONTRACT_ID), object_attribute(OATTR_CONTRACT_ID),
#           assign(AEON_CHANNEL_ID, UATTR_CONTRACT_ID), assign(UATTR_CONTRACT_ID, SERVICE_PROVIDER_ID),
#           assign(DEVICE_ID, OATTR_CONTRACT_ID), assign(OATTR_CONTRACT_ID, OWNER_ID),
#           cond( COND_PRED_INVOCATION, associate(UATTR_CONTRACT_ID, [r], OATTR_CONTRACT_ID) ) ] )
#
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=mpolicy" --data-urlencode "policy_elements=[
	user_attribute(ua_cntr_5f3fa521b1782447069c2649),
	object_attribute(oa_cntr_5f3fa521b1782447069c2649),
	assign(achnl_5f5a39f20463e50012bca2c3, ua_cntr_5f3fa521b1782447069c2649),
	assign(ua_cntr_5f3fa521b1782447069c2649, sp_5f1aa9f638189e22005d0f39),
	assign('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4', oa_cntr_5f3fa521b1782447069c2649),
	assign(oa_cntr_5f3fa521b1782447069c2649, owner_1),
	cond(dr_offer_5f5a39f2b559dcf200f424d0(devid,mchan,tstart,tstop,tsubmit,loMin,loMax,laMin,laMax),
		associate(ua_cntr_5f3fa521b1782447069c2649, [r], oa_cntr_5f3fa521b1782447069c2649) )
	]" --data-urlencode "token=admin_token"

echo 'read the policy (explicitly named as mpolicy)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=mpolicy" --data-urlencode "token=admin_token"

echo 'query the policy for aeon channels eligible to receive a datapackage arriving from the owner device'
# Stage 5 -- make query for received datapackage
# template: users( ar=r, cond=[NAMED_ARG1=VAL1, NAMED_ARG2=VAL2, ..., NAMED_ARGn=VALn], object=DEVICE_ID )
#
curl -s -G "http://127.0.0.1:8001/pqapi/users" --data-urlencode "ar=r" --data-urlencode "cond=[
	devid='95b40cf9-a9fc-4bd8-b695-99773b6f25e4', mchan=2,
	tstart='2020-09-08T08:00:00Z', tstop='2020-09-09T08:00:00Z', tsubmit='2020-09-09T08:03:22.350069Z',
	loMin=3.419216, loMax=3.519216, laMin=40.062069, laMax=40.072069
	]" --data-urlencode "object=device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4"


echo 'reset conditions before other tests are run'
curl -s -G "http://127.0.0.1:8001/paapi/resetcond" --data-urlencode "token=admin_token"

echo end marketplace demo tests
