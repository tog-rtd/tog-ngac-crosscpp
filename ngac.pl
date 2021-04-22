% Next Generation Access Control policy tool

:- module(ngac, [ngac/0,ngac/1,ngac/4,ngac_server/0,epp_server/0]).
:- use_module([
       param,command,pio,policies,
       test,procs,pmcmd,
       dpl,server,pqapi,paapi,pap,pdp,jsonresp,
       % pip,
       sessions,domains,
       audit,
       erl,epp, epp_cpa,epp_era,dpl_conditions, eppapi
   ]).

:- style_check(-singleton).

% :- initialization(ngac).
%

:- set_prolog_flag(verbose, silent).

% These are the main entry points to the system
% Other special entry points may also be defined here
%
ngac :- % most typical entry
	get_command_args(_Argv),
	ngac(_,_,_,_), !.
ngac :- halt(1).

% can be invoked with directives: (could do to allow a set of directives)
ngac(self_test) :- !, ngac(on,off,on,_).
ngac(regression_test) :- !, ngac(off,on,on,_).
ngac(no_initial) :- !, ngac(off,off,off,_).
ngac(verbose) :- !, ngac(_,_,_,on).

ngac(Selftest,Regression,Init,Verbose) :-
	(   var(Selftest) -> param:self_test(Selftest) ; true ),
	(   var(Regression) -> param:regression_test(Regression) ; true),
	(   var(Init) -> param:initialize(Init) ; true ),
	(   var(Verbose) -> param:verbose(Verbose) ; true ),

	(   Verbose == on
	-> format('self_test=~a regression_test=~a initialize=~a verbose=~a~n',
		  [Selftest,Regression,Init,Verbose])
	; true),

	(   Init == on
	-> initialize_all
	; true ),

	(   Selftest == on
	->  self_test_all
	;   true ),

	(   Regression == on
	->  regression_test_all
	;   true ),

	(   param:guitracer(on)
	->  guitracer
	;   true ),

	param:prompt_string(Prompt),
	command:tl(Prompt). % run the top-level ngac command interpreter

ngac_server :-
	get_command_args(Argv),
	initialize_all,
	server:server_with_args(Argv).

epp_server :-
	get_command_args(Argv),
	initialize_all,
	epp:epp_with_args(Argv).

get_command_args(Argv) :-
	current_prolog_flag(argv, Argv),
	% format('Argv: ~q~n',[Argv]),
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialization
%
initialize_once :- param:initialized(true), !.
initialize_once :-
	open_null_stream(Null), param:setparam(null_stream,Null),
	true.

initialize_all :-
	% initialize all subsystems and modules requiring startup initialization
	% ui:notify(initialize,all),
        initialize_once,
	audit:init(full), % basic or full
	dpl:init,
	% following should properly be conditional on param:epp_status
	% and how the server and epp are started
	% epp:init,
	% ...

	param:setparam(initialized,true).

% Test
%
self_test_all :-
	test:self_test,
	true.

regression_test_all :-
	test:regression_test,
	true.
