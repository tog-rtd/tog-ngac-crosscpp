% stored "built-in" procedures

:- module(procs, [proc/2, pmproc/2]).

:- dynamic([proc/2, pmproc/2]).

%%	NGAC Command Procs
% sequences of commands defined in command module

proc(review, [ %
         proc(queryA),
         proc(queryB),
         proc(autoCombined),
	 selftest,
         server(8001)
     ]).

proc(queryA, [ % query the example Policy (a)
	 newpol('Policy (a)'),
         display_policy,
	 access('Policy (a)',(u1,r,o1)),
	 access('Policy (a)',(u1,w,o1)),
	 access('Policy (a)',(u1,r,o2)),
	 access('Policy (a)',(u1,w,o2)),
	 access('Policy (a)',(u1,r,o3)),
	 access('Policy (a)',(u1,w,o3)),
	 access('Policy (a)',(u1,r,o4)),
	 access('Policy (a)',(u1,w,o4)),
	 access('Policy (a)',(u2,r,o1)),
	 access('Policy (a)',(u2,w,o1)),
	 access('Policy (a)',(u2,r,o2)),
	 access('Policy (a)',(u2,w,o2)),
	 access('Policy (a)',(u2,r,o3)),
	 access('Policy (a)',(u2,w,o3)),
	 access('Policy (a)',(u2,r,o4)),
	 access('Policy (a)',(u2,w,o4)),
         echo('Displaying policy graph'),
         graph_policy
     ]).

proc(queryB, [ % query the example Policy (b)
	 newpol('Policy (b)'),
         display_policy,
	 access('Policy (b)',(u1,r,o1)),
	 access('Policy (b)',(u1,w,o1)),
	 access('Policy (b)',(u1,r,o2)),
	 access('Policy (b)',(u1,w,o2)),
	 access('Policy (b)',(u1,r,o3)),
	 access('Policy (b)',(u1,w,o3)),
	 access('Policy (b)',(u1,r,o4)),
	 access('Policy (b)',(u1,w,o4)),
	 access('Policy (b)',(u2,r,o1)),
	 access('Policy (b)',(u2,w,o1)),
	 access('Policy (b)',(u2,r,o2)),
	 access('Policy (b)',(u2,w,o2)),
	 access('Policy (b)',(u2,r,o3)),
	 access('Policy (b)',(u2,w,o3)),
	 access('Policy (b)',(u2,r,o4)),
	 access('Policy (b)',(u2,w,o4)),
         echo('Displaying policy graph'),
         graph_policy
     ]).

proc(autoCombined, [
	 import(policy('EXAMPLES/policy_signals_access.pl')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Trip Signals')),
	 import(policy('EXAMPLES/policy_vehicle_ownership.pl')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Trip Signals')),
	 combine('Signals Access Policy','Vehicle Ownership Policy','Combined Policy'),
	 newpol('Combined Policy'),
         echo('Using Combined Policy'),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Sebastian',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Trip Signals')),
         graph_policy
     ]).

proc(modelTestA, [
	 newpol('Policy (a)'),
	 los('Policy (a)'),
	 dps('Policy (a)'),
	 aoa(u1),
	 minaoa(u1),
	 userlos('Policy (a)',u1),
	 aoa(u2),
	 minaoa(u2),
	 userlos('Policy (a)',u2)
     ]).

proc(modelTestB, [
	 newpol('Policy (b)'),
	 los('Policy (b)'),
	 dps('Policy (b)'),
	 aoa(u1),
	 minaoa(u1),
	 userlos('Policy (b)',u1),
	 aoa(u2),
	 minaoa(u2),
	 userlos('Policy (b)',u2)
     ]).

proc(demo1, [ %
	 import(policy('EXAMPLES/policy3.pl')),
	 newpol('Policy3'),
	 access('Policy3',(jones,read,mrec1)),
	 access('Policy3',(jones,write,mrec1)),
	 access('Policy3',(smith,read,mrec1)),
	 access('Policy3',(smith,write,mrec1)),
	 access('Policy3',(smith,read,'Medical Records')) % OA not Object
     ]).

proc(demo2, [ % convert a declarative policy file to a PM command file
	 import(policy('EXAMPLES/Policy3.pl')),
	 newpol('Policy3'),
	 decl2imp('EXAMPLES/policy3.pl','EXAMPLES/policy3.pm')
     ]).

proc(demo3, [ %
	 combine('Policy (a)','Policy (b)','Policy (ab)'),
	 dps('Policy (ab)')
     ]).

proc(test_echo, [
	 echo('hello world')
     ]).

proc(auto1, [
	 import(policy('EXAMPLES/policy_signals_access.pl')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

proc(auto2, [
	 import(policy('EXAMPLES/policy_vehicle_ownership.pl')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

proc(autocomb, [
	 combine('Signals Access Policy','Vehicle Ownership Policy','Combined Policy'),
	 newpol('Combined Policy'),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Sebastian',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Trip Signals')), % <==
	 access('Combined Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

