% COMMAND INTERPRETER FOR BASIC USER INTERFACE
% and definition of the 'ngac' commands

:- module(command, [ tl/0, tl/1 ]).

:- use_module(param).
:- use_module(ui).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool command syntax
%
% syntax( Signature, UserMode )
%
% there must be a syntax entry for every command form
% do not create a 0-ary command with the name "invalid" (see rd/2)
%
% Added value 'developer' for commands such as inspect,regtest,reinit
%
syntax(access(policy,(user,mode,object)),           admin).
syntax(access(policy,(user,mode,object),condition), admin).
syntax(activate_erp(erp_name),			    admin).
syntax(admin,                                       admin).
syntax(advanced,                                    admin).
syntax(aoa(user),				    admin).
syntax(aua(object),				    admin).
syntax(combine(p1,p2,p3),			    admin).
syntax(conditions,			            admin).
syntax(conditions(name),		            admin).
syntax(current_erp,				    admin).
syntax(deactivate_erp(erp_name),		    admin).
syntax(decl2imp(decl_file,imp_file),		                                  obsolete).
syntax(demo(demo_command),                          admin).
syntax(dps(policy),                                                               obsolete).
syntax(echo(string),                                admin).
syntax(epp(port),				    admin).
syntax(epp(port,token),			            admin).
syntax(export_commands(imp_file),                                                 obsolete).
syntax(getpol,				            admin).
syntax(guitracer,			            admin).
syntax(guiserver,			            admin).
syntax(halt,                                        admin).
syntax(help,                                        admin).
syntax(help(command),				    admin).
syntax(import(file_spec),                                     advanced).
syntax(import_policy(policy_file),	                      advanced).
syntax(inspect,                                                         developer).
syntax(inspect(item),                                                   developer).
syntax(load_erf(erp_file),			              advanced).
syntax(load_cond(cond_file),			              advanced).
syntax(load_cond(cond_name,cond_file),			      advanced).
syntax(los(policy),                                                               obsolete).
syntax(make,                                                            developer).
syntax(minaoa(user),				                                  obsolete).
syntax(newpol(policyid),                            admin).
syntax(noop,					    admin).
syntax(nl,                                          admin).
syntax(pmcmd,                                                                     obsolete).
syntax(policy_graph,				    admin).
syntax(policy_graph(policy),			    admin).
syntax(policy_graph(policy,graph_file),	            admin).
syntax(policy_spec,                                 admin).
syntax(policy_spec(policy),                         admin).
syntax(policy_spec(policy,policy_file),	            admin).
syntax(policy_spec(policy,policy_file,silent),      admin).
syntax(proc(proc_id),                               admin).
syntax(proc(proc_id,step_or_verbose),		    admin).
syntax(quit,                                        admin).
syntax(regtest,								developer).
syntax(reset,					    admin).
syntax(reset(domain,name),                          admin).
syntax(reinit,                                                          developer).
syntax(script(file),                                admin).
syntax(script(file,step_or_verbose),		    admin).
syntax(selftest,					      advanced).
syntax(server,				            admin).
syntax(server(port),				    admin).
syntax(server(port,atoken),			    admin).
syntax(server(port,atoken,etoken),		    admin).
syntax(set,						      advanced).
syntax(set(name),				              advanced).
syntax(set(name,value),				              advanced).
syntax(setpol(policyid),                            admin).
syntax(status,				            admin).
syntax(step,								developer).
syntax(step(number_of_steps),						developer).
syntax(store(pol_id),				              advanced).
syntax(time(command),                                         advanced).
syntax(time(command,repeat),			              advanced).
syntax(traceoff,					                developer).
syntax(traceon,					                        developer).
syntax(traceone,					                developer).
syntax(unload_erp(erp_name),			              advanced).
syntax(userlos(policy,user),                        admin).
syntax(users(object),                               admin).
syntax(users(object,mode),                          admin).
syntax(users(object,mode,condition),                admin).
syntax(version,				            admin).
syntax(versions,				    admin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool command semantics
%
% optional static semantics entry, e.g., used to check command arguments
% distinct from syntax so syntax can be called separately
semantics(access(P,(U,M,O))) :- !, ground(P), ground(U), ground(M), ground(O).
semantics(access(P,(U,M,O),C)) :- !, ground(P), ground(U), ground(M), ground(O),
	(   C==true ; compound(C) ; is_list(C) ), !.
semantics(activate_erp(Erp_name)) :- !, atom(Erp_name).
semantics(aoa(U)) :- !, ground(U).
semantics(aua(O)) :- !, ground(O).
semantics(combine(P1,P2,P3)) :- !, atom(P1), atom(P2), atom(P3).
semantics(conditions(N)) :- !, atom(N).
semantics(deactivate_erp(Erp_name)) :- !, atom(Erp_name).
semantics(decl2imp(Dfile,Ifile)) :- !, atom(Dfile), atom(Ifile).
semantics(demo(C)) :- !, ground(C).
semantics(dps(P)) :- !, ground(P).
semantics(echo(S)) :- !, atomic(S).
semantics(epp(Port)) :- !, integer(Port).
semantics(epp(Port,Token)) :- !, integer(Port), atom(Token).
semantics(export_commands(C)) :- atom(C).
semantics(graph_policy(P)) :- !, atom(P).
semantics(help(C)) :- !, ground(C).
semantics(import(FS)) :- !, functor(FS,F,1), (F==policy ; (F==model ; (F==pm ; F==database ; F==erp))).
semantics(import_model(M)) :- atom(M).
semantics(import_pm(PM)) :- atom(PM).
semantics(import_policy(P)) :- atom(P).
semantics(inspect(I)) :- nonvar(I).
semantics(load_erf(ERF)) :- atom(ERF).
semantics(load_cond(CondF)) :- atom(CondF).
semantics(load_cond(CondN,CondF)) :- atom(CondN), atom(CondF).
semantics(los(P)) :- !, ground(P).
semantics(minaoa(U)) :- !, ground(U).
semantics(newpol(ID)) :- !, ground(ID).
semantics(newpol(T,ID)) :- !, ground(ID), ground(T). % synonym for setpol
semantics(policy_graph(P)) :- !, atom(P).
semantics(policy_graph(P,F)) :- !, atom(P), atom(F).
semantics(policy_spec(P)) :- !, atom(P).
semantics(policy_spec(P,F)) :- !, atom(P), atom(F).
semantics(policy_spec(P,F,S)) :- !, atom(P), atom(F), S == silent.
semantics(proc(P)) :- !, atom(P).
semantics(proc(P,Opt)) :- !, atom(P), (Opt==step;Opt==s;Opt==verbose;Opt==v). % other opts can be added
semantics(reset(Dom,Name)) :- !, atom(Dom), atom(Name).
semantics(script(F)) :- !, atom(F).
semantics(script(F,Opt)) :- !, atom(F), (Opt==step;Opt==s;Opt==verbose;Opt==v). % other opts can be added
semantics(server(Port)) :- !, integer(Port).
semantics(server(Port,AToken)) :- !, integer(Port), atom(AToken).
semantics(server(Port,AToken,EToken)) :- !, integer(Port), atom(AToken), atom(EToken).
semantics(set(N)) :- !, atom(N).
semantics(set(N,V)) :- !, atom(N), ground(V).
semantics(setpol(ID)) :- !, ground(ID).
semantics(step(N)) :- !, (integer(N) ; N == break), !.
semantics(store(P)) :- !, atom(P).
semantics(time(C)) :- !, ground(C).
semantics(time(C,N)) :- !, ground(C), integer(N).
semantics(unload_erp(ERP)) :- atom(ERP).
semantics(userlos(P,U)) :- !, ground(P), ground(U).
semantics(users(O)) :- !, atom(O).
semantics(users(O,M)) :- !, atom(O), atom(M).
semantics(users(O,M,C)) :- !, atom(O), atom(M),(atom(C);compound(C);is_list(C)),!.
semantics(_). % succeed for all other commands

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command help strings
%   all strings for a given key are displayed when key is given as an
%   argument to the help command, e.g., "help(assess)"
%
%   help(Key,    HelpString)
help(access,    'Under policy, user can access in mode the object.').
help(access,	'Arg1 is a policy name.').
help(access,    'Arg2 is and access triple, "(User, Mode, Object)".').
help(access,    'Arg3 (opt) a condition predicate for conditional rules.').

help(activate_erp, 'Activate an event-response package alreadly loaded.').
help(activate_erp, 'Arg is the name of a loaded ER package.').

help(admin,     'Switch to admin user mode, limiting available commands to admin command set.').
help(advanced,  'Switch to advanced user mode, enabling all commands.').

help(aoa,	'all object attributes for user in current policy and policy class').
help(aoa,       'Arg is user identifier.').

help(aua,	'all user attributes for object in current policy and policy class').
help(aua,       'Arg is object identifier.').

help(combine,	'Arg1 and Arg2 are the names of currently loaded declarative policy specs.').
help(combine,	'Arg3 is name of a new policy spec that is the combination of the first two.').

help(conditions,'Display current condition variable and condition predicate declarations.').
help(conditions,'Arg1 (optional) the name of the conditions to display (predefined/static/dynamic/...).').

help(current_erp, 'Display the name of the current active ER package.').

help(deactivate_erp, 'Deactivate an event-response package alreadly loaded.').
help(deactivate_erp, 'Arg is the name of a loaded ER package.').

help(decl2imp,	'Arg1 is name of input file containing declarative policy spec.').
help(decl2imp,	'Arg2 is name of output file to contain imperative policy spec.').

help(demo,	'Run canned demos.'). % command for running canned demos of different features
help(demo,      'Arg is demo identifier.').

help(dps,       'Show derived privileges of the specified policy').
help(dps,	'Arg is a policy name').

help(echo,      'echo a (single-quoted) string argument.').

help(epp,	'Start the event processing point.').
help(epp,       'Arg1 is the port number.').
help(epp,       'Arg2 (optional) is an admin token.').

help(export,    'export a specified model(model_id), policy(type,attrs) or database(db_id)').

help(export_policy, '"export" a policy for consumption by external tools').
help(export_model, '"export" a model for comsumption by external tools').

help(getpol,    'Show the name of the current policy.').

help(halt,	'Leave NGAC command loop and Prolog.').

help(help,	'"help" with no argument lists the legal command forms.').
help(help,	'With a command name as argument it provides help on that command.').

help(import,    'import a specified policy policy(file), pm(file), erp(file).').

help(import_pm, '"import" a policy in PM imperative commands from a file').
help(import_policy, '"import" a declarative policy from a file.').

help(inspect,	'Inspect values of internal structures or variables based on arg.').
help(inspect,	'arg options: settings, xml, str, prm, current or other structures.').
help(inspect,	'arg: target(<target>,<element>) will show intermediate facts.').

help(load_erf, '"load" an event-response package from a file').

help(load_cond, '"load" condition declarations and definitions from a file').
help(load_cond, 'Arg1 is the conditions file to be loaded.').
help(load_cond, 'Arg2 (opt) is the name to be associated with conditions (default "dynamic").').

help(los,       'Show logical object system of the specified policy').
help(los,	'Arg is a policy name').

help(make,	'Recompile changed source files.').

help(newpol,	'Create a new policy as the "current policy".').
help(newpol,	'If two arguments (currently unimplemented) the first is policy type.').
help(newpol,	'Last argument is the policy ID.').
help(newpol,	'This is a deprecated synonym for setpol.').

help(nl,        'Write a newline to the console.').

help(pmcmd,	'Enter PM server command mode.').

help(policy_graph, 'Display graph of the current or named policy,').
help(policy_graph, 'Arg1 (opt) names a currently loaded policy (or "current_policy") to graph,').
help(policy_graph, 'Arg2 (opt) file name root for dot and png files.').

help(policy_spec, 'Display the current or named policy,').
help(policy_spec, 'Arg1 (opt) names a currently loaded policy (or "current_policy") to display,').
help(policy_spec, 'Arg2 (opt) file name root to save policy file,').
help(policy_spec, 'Arg3 (opt) if present, must be "silent" to create file without console display.').

help(proc,	'Run a stored NGAC command procedure.').
help(proc,	'Arg 1 is a procedure identifier.').
help(proc,      'Arg 2 (optional) is "step" or "verbose".').

help(quit,	'Terminate the NGAC top-level command loop or a command script; stay in Prolog.').

help(regtest,   'Run regression tests.').

help(reinit,	'Re-initialize.').

help(reset,     'Reset policy or condition databases.').
help(reset,	'Arg 1 is the domain (conditions or policy) to be reset.').
help(reset,     'Arg 2 is the name of the group to be reset.').

help(script,	'Run a NGAC command script from a file.').
help(script,	'Arg 1 is the file name.').
help(script,	'Arg 2 (optional) is "step" or "verbose".').

help(selftest,  'Run self tests.').

help(server,	'Start the lightweight policy server.').
help(server,    'Arg1 (optional) is the port number.').
help(server,    'Arg2 (optional after Arg1) is a Server admin token.').
help(server,    'Arg3 (optional after Arg2) is an EPP token.').

help(set,	'With no argument displays all settable parameters.').
help(set,	'Arg 1 is name of a paramater. If only one arg, display its value.').
help(set,	'Arg 2, if present, is the value to set.').
help(set,       'Settable: cache, debug, initialize, statusprt, self_test, regression_test, verbose.').

help(setpol,	'Create a new policy as the "current policy".').
help(setpol,	'If two arguments (currently unimplemented) the first is policy type.').
help(setpol,	'Last argument is the policy ID.').

help(status,	'Display NGAC system status.').

help(step,	'"step" with no argument steps engine one cycle.').
help(step,	'With an integer argument it steps the engine that number of cycles.').

help(time,	'Execute command and report time stats.').
help(time,	'With an integer second argument, execute command repeatedly and report total time stats.').

help(traceoff,  'Turn Prolog tracing off.').
help(traceon,   'Turn Prolog tracing on.').
help(traceone,	'Turn Prolog tracing on for one NGAC command.').

help(unload_erp, '"unload" an event-response package').

help(userlos,   'Show the logical object system under policy for user.').
help(userlos,	'Arg 1 is policy name.').
help(userlos,   'Arg 2 is user name.').

help(users,     'List users with access to object.').
help(users,     'Arg1 is an object.').
help(users,     'Arg2 (optional) is an access mode to the object.').
help(users,     'Arg3 (optional after Arg2) is a condition predicate.').

help(version,	'Show current version number.').
help(versions,	'Show past versions with descriptions and current version.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% do the command, should be one for every implemented valid command form
% known broken or unimplemented commands should just "fail." straightaway
% interactive_do provides an appropriate message for interactive use of
% a command that is known to be invalid because it fails syntax or
% semantics check, does not have a do entry, or fails in do.
% (Would be better to distinguish between unimplemented and failed do,
% which is what the fail_act in tl is for.)
% As it is now, commands with an entry in do that fail are reported
% in the interactive_do as unimplemented commands.
%
do(access(P,(U,M,O))) :- !,
	(   pdp:access_check(P,(U,M,O))
	->  writeln(grant)
	;   writeln(deny)
	).
do(access(P,(U,M,O),C)) :- !,
	(   pdp:access_check(P,(U,M,O),C)
	->  writeln(grant)
	;   writeln(deny)
	).
do(activate_erp(ERPname)) :- !, epp_era:activate_loaded_erp(ERPname).
do(admin) :- !, retractall(user_mode(_)), assert(user_mode(admin)).
do(advanced) :- !, retractall(user_mode(_)), assert(user_mode(advanced)).
do(aoa(U)) :- !, param:current_policy(P), dpl:policy(P,PC),
	pdp:aoa(P,U,PC,AOA), ui:display_list(AOA).
do(aua(O)) :- !, param:current_policy(P), dpl:policy(P,PC),
	pdp:aua(P,O,PC,AUA), ui:display_list(AUA).
do(combine(P1,P2,Presult)) :- !,
	pap:compose_policies(P1,P2,Presult),
	true.
do(conditions) :- !, pio:display_conditions.
do(conditions(N)) :- !, pio:display_conditions(N).
do(current_erp) :- !, param:current_erp(Erp), writeln(Erp).
do(decl2imp(D,I)) :- !,
	 % same as import_policy+export_commands w/o making current policy
	dpl:decl2imp(D,I).
do(demo(C)) :- !, perform_demo(C).

do(deactivate_erp(ERPname)) :- !, epp_era:deactivate_loaded_erp(ERPname).
do(dps(P)) :- !, %param:current_policy(P), % dpl:policy(P,PC),
	pdp:policy_dps(P,DPS), ui:display_list(DPS).
do(echo(S)) :- !, writeln(S).

do(epp(Port)) :- !, epp:epp(Port).
do(epp(Port,Token)) :- !, epp:epp(Port,Token).

do(export(commands,CmdFile)) :- !,
	param:current_policy(PolicyName),
	do( export_commands(PolicyName,CmdFile) ).
do(export_commands(PolicyName,CmdFile)) :-
	dpl:save_as_cmds(PolicyName,CmdFile).

do(getpol) :- !, param:current_policy(P), writeq(P), nl.

do(guitracer) :- !,
	(   param:guitracer(off)
	->  setparam(guitracer,on),
	    guitracer
	;   true).

do(guiserver) :- !,
	(   param:guiserver(off)
	->  do(set(guiserver,on)),
	    do(guitracer),
	    do(set(jsonresp_server,on)), % turns on JSON responses for policy server
	    do(set(jsonresp,on)),
	    do(set(no_sleep,on)),
	    % do(traceone),
	    do(server(8001)),
	    do(echo(ready))
	;   do(echo('already on'))
	).

do(help) :- help.
do(help) :- !.
do(help(C)) :- !, show_help(C).

do(import(erp(ERLfile))) :- do(load_erp(ERLfile)).
do(import(pm(PM))) :- do(import_pm(PM)).
do(import(policy(P))) :- do(import_policy(P)).

do(import_pm(PM)) :- % import a PM imperative command file
	pio:load_CmdTerms_from_CmdStrFile(PM,CmdTerms),
	% create policy form
	dpl:cmdTerms2policy(CmdTerms,Policy),
	% make it a queryable policy
	dpl:unpack_policy(Policy).
do(import_policy(Pfile)) :-  % import declarative policy
	dpl:load_decl_policy(Pfile,PolicyName),
	ui:notify('Policy loaded',PolicyName),
	do( newpol(PolicyName) ).

do(inspect) :- !, writeln('inspect(opt). options: settings').
do(inspect(Item)) :- !, inspect(Item).

do(load_erf(ERLfile)) :- % load an event-response package from file
	epp:load_erp(ERLfile,ERPname),
	ui:notify('Event-Response Package loaded',ERPname).

do(load_cond(CondFile)) :- !, % load conditions from file
	do( load_cond(CondFile,dynamic) ).

do(load_cond(CondFile,CondName)) :- !, % load conditions from file
	exists_file(CondFile), read_file_to_terms(CondFile,CondTerms,[]),
	format('CondTerms read:~q~n',[CondTerms]),
	pap:dynamic_add_cond_elements(CondName,CondTerms),
	format(atom(M),'Conditions file ~q loaded',[CondFile]),
	ui:notify(CondName,M).

do(los(P)) :- !, pdp:los(P,LOS), ui:display_list(LOS).
do(make) :- !, make.
do(minaoa(U)) :- !, param:current_policy(P), dpl:policy(P,PC),
	pdp:min_aoa(P,U,PC,MAOA), ui:display_list(MAOA).
do(newpol(P)) :- !, do(setpol(P)).
do(noop) :- !.
do(nl) :- nl.
do(pmcmd) :- !, (interactive(true) -> tl(pmcmd) ; true).

do(policy_graph) :- !, do(policy_graph(current_policy)).
do(policy_graph(current_policy)) :- !, param:current_policy(P), do(policy_graph(P)).
do(policy_graph(current_policy,Fileroot)) :- !, param:current_policy(P), do(policy_graph(P,Fileroot)).
do(policy_graph(P)) :- !, param:graph_tmp_file(T), do(policy_graph(P,T)).
do(policy_graph(P,Fileroot)) :- !, dpl:policy(P,_),
	(   exists_directory('GRAPHS')
	->  true
	;   make_directory('GRAPHS')
	),
	atomic_list_concat(['GRAPHS/',Fileroot,'.dot'],DOTfile),
	atomic_list_concat(['GRAPHS/',Fileroot,'.png'],PNGfile),
	current_output(Old),
	open(DOTfile,write,DOT,[create([default])]),
	set_output(DOT),
	pio:graph_policy(P),
	close(DOT,[force(true)]),
	set_output(Old),
	atomic_list_concat(['dot -Tpng ', DOTfile , ' >', PNGfile], DotCommand),
	atomic_concat('open ', PNGfile, OpenCommand),
	shell(DotCommand,_), shell(OpenCommand,_),
	(   param:graph_tmp_file(Fileroot)
	->  sleep(2), delete_file(DOTfile), delete_file(PNGfile)
	;   true
	).

do(policy_spec) :- !, do(policy_spec(current_policy)).
do(policy_spec(current_policy)) :- !, param:current_policy(P), do(policy_spec(P,no_file_output,false)).
do(policy_spec(current_policy,Fileroot)) :- !, param:current_policy(P), do(policy_spec(P,Fileroot,false)).
do(policy_spec(current_policy,Fileroot,Silent)) :- !, param:current_policy(P), do(policy_spec(P,Fileroot,Silent)).
do(policy_spec(P)) :- !, do(policy_spec(P,no_file_output,false)).
do(policy_spec(P,Fileroot)) :- !, do(policy_spec(P,Fileroot,false)).
do(policy_spec(P,Fileroot,Silent)) :- !, dpl:policy(P,_),
	(   Fileroot \== no_file_output
	->  (   exists_directory('POLICIES')
	    ->  true
	    ;   make_directory('POLICIES')
	    ),
	    atomic_list_concat(['POLICIES/',Fileroot,'.pl'],PLfile),
	    current_output(Old),
	    open(PLfile,write,PL,[create([default])]),
	    set_output(PL),
	    pio:display_policy(P),
	    close(PL,[force(true)]),
	    set_output(Old)
	;   true
	),
	(   Silent \== silent
	->  pio:display_policy(P)
	;   true
	).

do(proc(Pid)) :- !, do(proc(Pid,none)).
do(proc(Pid,Opt)) :- !, procs:proc(Pid,Proc), param:prompt_string(P),
	retractall(interactive(_)), assert(interactive(false)),
	run_commands(P,Proc,Opt),
	retractall(interactive(_)), assert(interactive(true)).
do(quit) :- !.
do(halt) :- !, halt.
do(regtest) :- !, ngac:regression_test_all.
do(reinit) :- !, dpl:re_init.
do(reset) :- !, pap:preset(conditions,all).
do(reset(D,N)) :- !, pap:preset(D,N).
do(script(F)) :- !, param:prompt_string(P), run_command_script(P,F,none).
do(script(F,Opt)) :- !, param:prompt_string(P), run_command_script(P,F,Opt).
do(selftest) :- !, ngac:self_test_all, /* others ... */ true.
do(set) :- !, param:settable_params(Ps), forall(member(P,Ps),do(set(P))). % display all settable params
do(set(P)) :- param:settable_params(Ps), member(P,Ps), !, % display a settable param
	Q =.. [P,V], call(param:Q), format('~a=~w~n',[P,V]).
do(set(_)) :- !, writeln('Unknown parameter name').
do(set(debug,V)) :- (V == on ; V == off), !, do(debug(V)).
do(set(statusprt,V)) :- (V == on ; V == off), !, do(statusprt(V)).
do(set(self_test,V)) :- (V == on ; V == off), !, do(self_test(V)).
do(set(P,V)) :- param:settable_params(Ps), member(P,Ps), !, param:setparam(P,V).

do(setpol(P)) :- !, pap:set_current_policy(P).
do(server) :- !, server:server.
do(server(Port)) :- !, server:server(Port).
do(server(Port,AToken)) :- !, server:server(Port,AToken).
do(server(Port,AToken,EToken)) :- !, server:server(Port,AToken,EToken).

do(set(initialize,V)) :- (V == on ; V == off), !, param:setparam(initialize,V).
do(set(regression_test,V)) :- (V == on ; V == off), !, param:setparam(regression_test,V).
do(set(verbose,V)) :- (V == on ; V == off), !, param:setparam(verbose,V).
do(set(policy,P)) :- !, do(newpol(P)).
% add cases for other parameter settings here
do(set(P,V)) :- atom(P), ground(V), param:setparam(P,V), !.
do(set(_,_)) :- !,
	writeln('Unknown parameter name or illegal parameter value').
do(status) :- param:ngac_name(N,_), write(N),
	writeln(' system status:'), ngac_status(status(S)), write_status(S).
do(time(Command)) :- !, time(do(Command)).
do(time(Command,N)) :- !,
	current_output(S), param:null_stream(Null), set_output(Null),
	time( (foreach(between(1,N,_), do(Command)), set_output(S)) ).
do(traceon) :-	retractall(tracing(_)), assert(tracing(on)), trace.
do(traceone) :-	retractall(tracing(_)), assert(tracing(set)).
do(traceoff) :- retractall(tracing(_)), assert(tracing(off)), notrace.

do(unload_erp(ERPname)) :- % load an event-response package from file
	epp:unload_erp(ERPname),
	ui:notify('Event-Response Package unloaded',ERPname).

do(userlos(P,U)) :- pdp:user_los(P,U,V,E),
	write('V='), ui:display_list(V,''), write('E='), ui:display_list(E,'').
do(users(O)) :-  !,
	param:current_policy(P), pdp:aua_users(P,O,_PC,Users), writeln(Users). %ui:display_list(Users).
do(users(O,M)) :- !,
	param:current_policy(P), pdp:aua_users(P,(M,O),Users), writeln(Users). %ui:display_list(Users).
do(users(O,M,true)) :-  do(users(O,M)).
do(users(O,M,C)) :- is_list(C), !, dpl_conditions:is_cond_var_def_list(C),
	param:current_policy(P),
	pdp:aua_users(P,O,_,M,C,Users), writeln(Users). %ui:display_list(Users).
do(users(O,M,C)) :- dpl_conditions:validate_condition_predicate(C,_), !,
	param:current_policy(P),
	writeln('condition predicate ignored'), % HERE more to do
	pdp:aua_users(P,(M,O),Users), writeln(Users). %ui:display_list(Users).
do(version) :- !,
	param:ngac_version(Cur), param:ngac_current_version_description(Desc),
	format('Current version: ~a: ~a~n',[Cur,Desc]).
do(versions) :- !,
	forall(param:ngac_version(V,D), format('~t~17|~a: ~a~n',[V,D])),
	do(version).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic tracing/1, user_mode/1, interactive/1.

% tracing
% values: on, off, set, and one
%         set changes to one in mid next iteration
%         one changes to off after the next command is run
tracing(off).

% user_mode
% values: admin or advanced
user_mode(advanced).

% interactive
% true when reading from user interaction
interactive(true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top-level command loop
%
% tl(CommandSet)
%
tl :- tl(ngac).


tl(ngac) :-
	param:user_level(Ulev), retractall(user_mode(_)), assert(user_mode(Ulev)),
	param:prompt_string(Prompt),
	banner,
	repeat,
	        pre_act, rd(Prompt,C), mid_act(C),
		(   interactive_do(ngac,C)
		->  true
		;   fail_act
		),
		post_act,
	(C == quit, ! ; fail).


banner :-
	param:ngac_version(V), param:name_string(NGAC_name),
	format('~n~a version ~a~n',[NGAC_name,V]),
	nl.

pre_act :- % do before reading the command
	true.
mid_act(_) :- % do after reading the command but before do-ing it
	(   tracing(set)
	->  retractall(tracing(_)),
	    assert(tracing(one)),
	    trace
	;   true
	).
post_act :- % do after performing the command or after fail_act
	(   tracing(one)
	->  retractall(tracing(_)),
	    assert(tracing(off)),
	    notrace
	;   true
	),
	(param:statusprt(on) -> do(status);true),
	nl, !.
fail_act :- % do when a command fails
	(   tracing(one)
	->  retractall(tracing(_)),
	    assert(tracing(off)),
	    notrace
	;   true
	),
	param:msg_failed_command(M),
	ui:notify('interactive',M).

interactive_do(_,invalid) :- !, unimplemented_command.
interactive_do(CS,C) :- param:prompt_string(CS), !, do(C).
interactive_do(CS,C) :-	 atom(CS), DO =.. [CS,C], !, call(CS:DO).
interactive_do(_,_) :- unimplemented_command.

unimplemented_command :- param:msg_unimplemented_command(M), writeln(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read and validate a command:
% execute a Prolog goal preceded by :- or ?-
% or check whether a valid NGAC command
% return invalid if not found or fails checks
%
rd(Prompt,C) :-
	atom_concat(Prompt,'> ',FullPrompt),
	read_history(h, '!h', [], FullPrompt, C, _Bindings),
	nonvar(C), % nonvar instead of ground to allow Prolog goals w/vars
	(   (C=..[:-,P];C=..[?-,P]) % command is a Prolog goal
	->  call(P), nl, !, fail    % bypass other goals in tl, repeat
	;   chk_command(Prompt,C)          % check the command, fail to 'invalid'
	), !.
rd(_,invalid).

chk_command(CommandSet,C) :-
	param:prompt_string(Prompt),
	(   CommandSet == Prompt
	->  syntax_chk(C),
	    semantics(C)
	;   Check =.. [cmd,C,_,_],
	    clause(CommandSet:Check,true)
	).

syntax_chk(C) :-
	functor(C,F,A), functor(Sig,F,A), user_mode(M),
	(   M == advanced
	->  syntax(Sig,_)
	;   syntax(Sig,M)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command scripts
%

run_command_script(CmdSet,F,Opt) :-
	(   access_file(F,read)
	->  (
	        read_file_to_terms(F,Commands,[]),
	        (   Opt == verbose
	        ->  param:msg_script_read(Mread), writeln(Mread),
		    ui:display_list(Commands,1),
	            param:msg_running_script(Mrun), writeln(Mrun)
	        ;   true
	        ),
	        run_commands(CmdSet,Commands,Opt)
	    )
	;
	    format('can''t find file "~a"~n', F)
	), !.

run_commands(_,[],_) :- !.
run_commands(CmdSet,[C|Cs],Opt) :-
	(
	    (	(Opt == step ; Opt == s)
	    ->	format('~n> ~q. ?', C), flush_output, readln(_)
	    ;	(Opt == verbose ; Opt == v)
	    ->	format('> ~q.~n', C)
	    ;	true
	    ),
	    (   (C=..[:-,P] ; C=..[?-,P]) % command is a Prolog goal
	    ->  call(P)
	    ;   % ground(C),
	        chk_command(CmdSet,C),          % check the command, fail to 'invalid'
		(   param:prompt_string(CmdSet)
		->  do(C)
		;   atom(CmdSet), DO =.. [CmdSet,C], call(CmdSet:DO)
		)
	    )
	    ;   format('~q : ',[C]),
		param:msg_failed_command(CM), writeln(CM),
		param:msg_script_aborted(SM), writeln(SM),
		Abort=true
	),
	((C == quit; Abort == true), ! ; run_commands(CmdSet,Cs,Opt)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command procedures
%   command procedures for miscellaneous commands, in some cases
%   procedures here may be temporary until they are moved to an
%   appropriate module
%
% may want to do something different for 'obsolete' and 'developer'
%

help :-
	user_mode(UM), !,
	writeln('<command> ::='),
	(   UM == advanced
	->  syntax(Sig,M)
	;   syntax(Sig,UM)
	),
	(   M \== obsolete  % don't show obsolete commands
	->  write('  '), write_canonical(Sig), nl
	;   true
	),
	fail.

show_help(C) :-
	C =.. [Command|_], % use only the command name, ignore args
	(   help(Command,_)
	->  nl, show_help_strings(Command),
	    (	( syntax(S,obsolete), S =.. [Command|_] )
	    ->	writeln('  OBSOLETE')
	    ;	true
	    )
	;   format('No help for command "~q"~n', Command)
	).

show_help_strings(Command) :-
	help(Command,HelpString), format('  ~a~n',HelpString), fail.
show_help_strings(_).

% inspection - for development and test
inspect(settings) :- !, do(set).
% add other inspect clauses here
%% - inspect(graph) :- !, graphmanager:getGraph(G),graphmanager:printGraph(G).
inspect(_) :- writeln('inspect: Unknown parameter name or illegal parameter value').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% status
%

ngac_status(status(NGAC)) :-
	NGAC = 'Blah blah blah'.

write_status(S) :- writeln(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% demos
%   to show-off implemented portions of functionality
%   insert perform_demo clauses for specific arguments following comment

perform_demo(X) :- unimpl_d(X).

unimpl_d(X) :- format('Unimplemented demo command: ~q~n',[X]).

