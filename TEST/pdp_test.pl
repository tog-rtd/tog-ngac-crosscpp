%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% self_test
pdp_startup_tests([tc01,tc02,tc03,tc04,tc05,tc06,tc07,tc08,
		    tc11,tc12,tc13,tc14,tc15,tc16,tc17,tc18,
		    tc21,tc22,tc23,tc24,tc25,tc26,tc27,tc28,
		    tc31,tc32,tc33,tc34,tc35,tc36,tc37,tc38,
		    /*tc40,tc41,*/tc42,tc43,/*tc44,*/tc45, /* no los, user_los, policy_dps */
                    tc50,tc51,tc52,tc53,tc54,tc55,tc56,tc57,tc58,tc59,
                    tc60,tc61,tc62,tc63,tc64,tc65,
		    /*tc71,tc72,tc73,tc74,tc75,tc76,tc77,tc78,
		    tc81,tc82,tc83,tc84,tc85,tc86,tc87,tc88,*/
                    tcsa00,tcsa01,tcsa02,tcsa03,tcsa04,tcsa06,tcsa07,
                    tcvo00,tcvo01,tcvo02,tcvo03,tcvo04,tcvo06,tcvo07,
                    tcvc00,tcvc01,tcvc02,tcvc03,tcvc04,tcvc05,tcvc06,tcvc07,tcvc08,tcvc09,
		    tcc01,tcc02,tcc03,tcc04,tcc05,tcc06,tcc07,tcc08,
		    tcc11,tcc12,tcc13,tcc14,tcc15,tcc16,tcc17,tcc18
		   ]).
pdp_regression_tests([]).

self_test :-
	pdp_startup_tests(Tests),
	forall(member(T,Tests), test:report_test(pdp:T)).

regression_test :-
	pdp_startup_tests(Startup),
	pdp_regression_tests(Regression),
	append(Startup,Regression,AllTests),
	forall(member(T,AllTests), test:report_test(pdp:T)).

tc01 :- access_check('Policy (a)',(u1,r,o1)).
tc02 :- access_check('Policy (a)',(u1,w,o1)).
tc03 :- access_check('Policy (a)',(u1,r,o2)).
tc04 :- \+ access_check('Policy (a)',(u1,w,o2)).
tc05 :- \+ access_check('Policy (a)',(u1,r,o3)).
tc06 :- \+ access_check('Policy (a)',(u1,w,o3)).
tc07 :- \+ access_check('Policy (a)',(u1,r,o4)).
tc08 :- \+ access_check('Policy (a)',(u1,w,o4)).

tc11 :- access_check('Policy (a)',(u2,r,o1)).
tc12 :- \+ access_check('Policy (a)',(u2,w,o1)).
tc13 :- access_check('Policy (a)',(u2,r,o2)).
tc14 :- access_check('Policy (a)',(u2,w,o2)).
tc15 :- access_check('Policy (a)',(u2,r,o3)).
tc16 :- access_check('Policy (a)',(u2,w,o3)).
tc17 :- \+ access_check('Policy (a)',(u2,r,o4)).
tc18 :- \+ access_check('Policy (a)',(u2,w,o4)).

tc21 :- \+ access_check('Policy (b)',(u1,r,o1)).
tc22 :- \+ access_check('Policy (b)',(u1,w,o1)).
tc23 :- access_check('Policy (b)',(u1,r,o2)).
tc24 :- access_check('Policy (b)',(u1,w,o2)).
tc25 :- \+ access_check('Policy (b)',(u1,r,o3)).
tc26 :- \+ access_check('Policy (b)',(u1,w,o3)).
tc27 :- \+ access_check('Policy (b)',(u1,r,o4)).
tc28 :- \+ access_check('Policy (b)',(u1,w,o4)).

tc31 :- \+ access_check('Policy (b)',(u2,r,o1)).
tc32 :- \+ access_check('Policy (b)',(u2,w,o1)).
tc33 :- access_check('Policy (b)',(u2,r,o2)).
tc34 :- access_check('Policy (b)',(u2,w,o2)).
tc35 :- access_check('Policy (b)',(u2,r,o3)).
tc36 :- access_check('Policy (b)',(u2,w,o3)).
tc37 :- access_check('Policy (b)',(u2,r,o4)).
tc38 :- access_check('Policy (b)',(u2,w,o4)).

tc40 :- los('Policy (a)',[user(u1),user(u2),object(o1),object(o2),object(o3),user_attribute('Division'),user_attribute('Group1'),user_attribute('Group2'),object_attribute('Gr2-Secret'),object_attribute('Project1'),object_attribute('Project2'),object_attribute('Projects'),policy_class('Project Access'),assign(u1,'Group1'),assign(u2,'Group2'),assign('Group1','Division'),assign('Group2','Division'),assign(o1,'Project1'),assign(o2,'Project2'),assign(o3,'Gr2-Secret'),assign('Project1','Projects'),assign('Project2','Projects'),assign('Division','Project Access'),assign('Projects','Project Access'),assign('Gr2-Secret','Project Access'),associate('Group1',[w],'Project1'),associate('Group2',[w],'Project2'),associate('Group2',[r,w],'Gr2-Secret'),associate('Division',[r],'Projects'),connector('PM')]).

tc41 :- policy_dps('Policy (a)',[('Group1',r,'Project1'),('Group1',r,'Project2'),('Group1',r,o1),('Group1',r,o2),('Group2',r,'Project1'),('Group2',r,'Project2'),('Group2',r,o1),('Group2',r,o2),(u1,r,'Project1'),(u1,r,'Project2'),(u1,r,o1),(u1,r,o2),(u1,w,o1),(u2,r,'Project1'),(u2,r,'Project2'),(u2,r,o1),(u2,r,o2),(u2,r,o3),(u2,w,o2),(u2,w,o3)]).

tc42 :- P='Policy (a)', U=u1, policy(P,PC), aoa(P,U,PC,['Project1', 'Project2', 'Projects', o1, o2]).

tc43 :- P='Policy (a)', U=u1, policy(P,PC), min_aoa(P,U,PC,['Projects']).

tc44 :- user_los('Policy (a)',u1,['PM','Project Access',o1,o2,'Project1','Project2','Projects'],[('Project Access','PM'),('Projects','Project Access'),(o1,'Project1'),(o2,'Project2'),	('Project1','Projects'),('Project2','Projects')]).

tc45 :- pap:compose_policies('Policy (a)','Policy (b)','Policy (ab)').
tc50 :- access_check('Policy (ab)',(u1,r,o1)).
tc51 :- access_check('Policy (ab)',(u1,w,o1)).
tc52 :- access_check('Policy (ab)',(u1,r,o2)).
tc53 :- access_check('Policy (ab)',(u2,r,o1)).
tc54 :- access_check('Policy (ab)',(u2,r,o2)).
tc55 :- access_check('Policy (ab)',(u2,w,o2)).
tc56 :- access_check('Policy (ab)',(u2,r,o3)).
tc57 :- access_check('Policy (ab)',(u2,w,o3)).
tc58 :- access_check('Policy (ab)',(u2,r,o4)).
tc59 :- access_check('Policy (ab)',(u2,w,o4)).

tc60 :- \+ access_check('Policy (ab)',(u1,w,o2)).
tc61 :- \+ access_check('Policy (ab)',(u1,r,o3)).
tc62 :- \+ access_check('Policy (ab)',(u1,w,o3)).
tc63 :- \+ access_check('Policy (ab)',(u1,r,o4)).
tc64 :- \+ access_check('Policy (ab)',(u1,w,o4)).
tc65 :- \+ access_check('Policy (ab)',(u2,w,o1)).

% these 70s and 80s tests assume p_uo variant of 'all'
% TODO find out why these stopped working
tc71 :- access_check('all',(u1,r,o1)).
tc72 :- access_check('all',(u1,w,o1)).
tc73 :- access_check('all',(u1,r,o2)).
tc74 :- \+ access_check('all',(u1,w,o2)).
tc75 :- \+ access_check('all',(u1,r,o3)).
tc76 :- \+ access_check('all',(u1,w,o3)).
tc77 :- \+ access_check('all',(u1,r,o4)).
tc78 :- \+ access_check('all',(u1,w,o4)).

tc81 :- access_check('all',(u2,r,o1)).
tc82 :- \+ access_check('all',(u2,w,o1)).
tc83 :- access_check('all',(u2,r,o2)).
tc84 :- access_check('all',(u2,w,o2)).
tc85 :- access_check('all',(u2,r,o3)).
tc86 :- access_check('all',(u2,w,o3)).
tc87 :- access_check('all',(u2,r,o4)).
tc88 :- access_check('all',(u2,w,o4)).

tcsa00 :- access_check('Signals Access Policy', ('Ana',r,'VIN-1001 Door Signals')).
tcsa01 :- access_check('Signals Access Policy', ('Ana',r,'VIN-3001 Shift Signals')).
tcsa02 :- \+ access_check('Signals Access Policy', ('Ana',r,'VIN-1001 Trip Signals')).
tcsa03 :- \+ access_check('Signals Access Policy', ('Ana',r,'VIN-3001 Trip Signals')).
tcsa04 :- \+ access_check('Signals Access Policy', ('Ana',w,'VIN-1001 Door Signals')).
tcsa05 :- \+ access_check('Signals Access Policy', ('Ana',w,'VIN-3001 Shift Signals')).
tcsa06 :- \+ access_check('Signals Access Policy', ('Ana',w,'VIN-1001 Trip Signals')).
tcsa07 :- \+ access_check('Signals Access Policy', ('Ana',w,'VIN-3001 Trip Signals')).

tcvo00 :- \+access_check('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Door Signals')).
tcvo01 :- access_check('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Shift Signals')).
tcvo02 :- \+ access_check('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Trip Signals')).
tcvo03 :- access_check('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Trip Signals')).
tcvo04 :- \+ access_check('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Door Signals')).
tcvo05 :- \+ access_check('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Shift Signals')).
tcvo06 :- \+ access_check('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Trip Signals')).
tcvo07 :- \+ access_check('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Trip Signals')).

tcvc00 :- pap:compose_policies('Signals Access Policy','Vehicle Ownership Policy','Vehicle Combined Policy').
tcvc01 :- \+ access_check('Vehicle Combined Policy', ('Ana',r,'VIN-1001 Door Signals')).
tcvc02 :- access_check('Vehicle Combined Policy', ('Sebastian',r,'VIN-1001 Door Signals')).
tcvc03 :- access_check('Vehicle Combined Policy', ('Ana',r,'VIN-3001 Shift Signals')).
tcvc04 :- \+ access_check('Vehicle Combined Policy', ('Ana',r,'VIN-1001 Trip Signals')).
tcvc05 :- \+ access_check('Vehicle Combined Policy', ('Ana',r,'VIN-3001 Trip Signals')).
tcvc06 :- \+ access_check('Vehicle Combined Policy', ('Ana',w,'VIN-1001 Door Signals')).
tcvc07 :- \+ access_check('Vehicle Combined Policy', ('Ana',w,'VIN-3001 Shift Signals')).
tcvc08 :- \+ access_check('Vehicle Combined Policy', ('Ana',w,'VIN-1001 Trip Signals')).
tcvc09 :- \+ access_check('Vehicle Combined Policy', ('Ana',w,'VIN-3001 Trip Signals')).

% need to manipulate the condition variables so these results are
% subject to controlled simulated conditions
%
tcc01 :- access_check('CondPolicy1',(u1,r,o1)).
tcc02 :- access_check('CondPolicy1',(u1,w,o1)).
tcc03 :- access_check('CondPolicy1',(u1,r,o2)).
tcc04 :- \+ access_check('CondPolicy1',(u1,w,o2)).
tcc05 :- \+ access_check('CondPolicy1',(u1,r,o3)).
tcc06 :- \+ access_check('CondPolicy1',(u1,w,o3)).
tcc07 :- \+ access_check('CondPolicy1',(u1,r,o4)).
tcc08 :- \+ access_check('CondPolicy1',(u1,w,o4)).

tcc11 :- access_check('CondPolicy1',(u2,r,o1)).
tcc12 :- \+ access_check('CondPolicy1',(u2,w,o1)).
tcc13 :- access_check('CondPolicy1',(u2,r,o2)).
tcc14 :- access_check('CondPolicy1',(u2,w,o2)).
tcc15 :- access_check('CondPolicy1',(u2,r,o3)). % succeeds on weekday == true
tcc16 :- access_check('CondPolicy1',(u2,w,o3)). % succeeds on weekday == true
tcc17 :- \+ access_check('CondPolicy1',(u2,r,o4)).
tcc18 :- \+ access_check('CondPolicy1',(u2,w,o4)).

