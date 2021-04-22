% Lightweight NGAC server

:- module(server, [server/1,server/2,server/3,server_with_args/1]).

:- use_module(audit,[audit_gen/2]).
:- use_module(param).
:- use_module(dpl).
:- use_module(sessions).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/http_wrapper)).
%:- use_module(library(http/http_header)).
%:- use_module(library(http/http_parameters)).

%
% ngac-server command line options
%
%    --deny    -d
%    --permit  --grant  -g
%    --port    --portnumber --pqport -p    <integer>
%    --import  --policy --load -i -l   <filename>
%    --selftest -s
%    --token   -t    <admintoken>
%    --verbose  -v
%    --jsonresp -j
%    --epp      -e
%
server_opt_spec([
        [opt(deny), type(boolean), default(false), shortflags([d]), longflags(['deny']),
         help( 'respond to all access requests with deny' )],
	[opt(grant), type(boolean), default(false), shortflags([g]), longflags(['permit','grant']),
         help( 'respond to all access requests with grant' )],
        [opt(pqportnumber), meta('QP'), type(integer), shortflags([p]), longflags(['port','portnumber','pqport']),
         help( 'server listens for policy queries on port QP' )],
 %       [opt(paportnumber), meta('AP'), type(integer), shortflags([a]), longflags(['admin','paport']),
 %        help( 'server listens for policy admin on port AP' )],
        [opt(importfile), meta('FILE'), type(atom), shortflags([i,l]), longflags(['import','policy','load']),
         help( 'import/load policy from FILE' )],
        [opt(selftest), type(boolean), default(false), shortflags([s]), longflags(['selftest']),
         help( 'run self tests on startup' )],
        [opt(token), meta('TOKEN'), type(atom), shortflags([t]), longflags(['token']),
         help( 'policy administration requests must cite TOKEN' )],
        [opt(verbose), type(boolean), default(false), shortflags([v]), longflags(['verbose']),
         help( 'show all messages' )],
        [opt(jsonresp), type(boolean), default(false), shortflags([j]), longflags(['jsonresp']),
         help( 'JSON-encoded responses' )],
        [opt(epp), type(boolean), default(false), shortflags([e]), longflags(['epp']),
         help( 'enable Event Processing Point' )],
        [opt(crosscpp), meta('URL'), type(atom), shortflags([c]), longflags(['crosscpp']),
	 help( 'URL of Cross-CPP Context Monitoring and Extraction system' )]
]).

:- dynamic server_options/1.
server_options([]).

% server/0, server/1, server/2 and server/3 are called by command:do
% server_with_args/1 is called by ngac:ngac_server
%
% should check if server is already running
%
server :-
	param:pqapi_port(Port), % use same port for pqapi and paapi
	server(Port).

server(Port) :-
	param:ngac_version(Vnum), format('ngac-server version ~a starting~n',Vnum),
	create_server_audit_log,
	(   param:guiserver(on)
	->  trace
	;   true
	),
	http_server(http_dispatch, [port(Port)]),
	format('ngac-server listening on port ~d~n',[Port]),
	audit_gen(ngac_start, success),
	param:setparam(epp_status,policy_server),
	epp:epp_with_server,
	(   param:no_sleep(off)
	->  param:server_sleeptime(S), go_to_sleep(S)
	;   true
	).

server(Port,AToken) :-
	param:setparam(admin_token,AToken),
	server(Port).

server(Port,AToken,EToken) :-
	param:setparam(epp_token,EToken),
	server(Port,AToken).

server_with_args(Argv) :-
	% process the arguments
	server_opt_spec(OptSpec),
	catch(
	    opt_parse(OptSpec,Argv,Opts,_Positionals),
	    E, writeln('error in command line arguments')),
	!,
	(   nonvar(E)
	->  halt(1)
	;   retractall(server_options(_)), assert(server_options(Opts))
	),
	server_with_opts(Opts).

server_with_opts(Opts) :-
	format('Options=~q~n',[Opts]),
	(   memberchk(pqportnumber(QPort),Opts); true ),
	(   var(QPort)
	->  param:pqapi_port(QPort)
	;   param:setparam(pqapi_port,QPort)
	),

%	(   memberchk(paportnumber(APort),Opts); true ),
%	(   var(APort)
%	->  param:paapi_port(APort)
%	;   param:setparam(paapi_port,APort)
%	),

	(   memberchk(crosscpp(CCPP_URL),Opts); true ),
	(   var(CCPP_URL)
	->  true % param:crosscpp_url(CCPP_URL)
	;   param:setparam(crosscpp_url,CCPP_URL)
	),

	(   memberchk(grant(true),Opts)
	->  param:setparam(current_policy,grant)
	;   true
	),

	(   memberchk(deny(true),Opts)
	->  (   memberchk(grant(true),Opts)
	    ->  writeln('grant and deny options cannot both be true--exiting'), halt(1)
	    ;   param:setparam(current_policy,deny)
	    )
	;   true
	),

	(   memberchk(verbose(true),Opts)
	->  param:setparam(verbose,on)
	;   param:setparam(verbose,off)
	),

	(   memberchk(jsonresp(true),Opts)
	->  param:setparam(jsonresp_server,on), % turns on JSON responses for policy server
	    param:setparam(jsonresp,on)
	;   param:setparam(jsonresp_server,off),
	    param:setparam(jsonresp,off)
	),

	(   memberchk(epp(true),Opts)
	->  param:setparam(epp_status,policy_server) % activate EPP as part of policy server
	;   true
	),

	(   memberchk(selftest(true),Opts) % currently ignored
	->  param:setparam(self_test,on)
	;   param:setparam(self_test,off)
	),

	(   memberchk(token(Token),Opts); true ),
	(   atom(Token)
	->  param:setparam(admin_token,Token)
	;   true
	),

	param:ngac_version(Vnum), format('ngac-server version ~a starting~n',Vnum),
	create_server_audit_log,
	http_server(http_dispatch, [port(QPort)]),
	format('ngac-server listening on port ~d~n',[QPort]),
	audit_gen(ngac_start, success),

	% run self-test here if turned on in param or command line
	% ngac:self_test_all,

	(   memberchk(importfile(Pfile),Opts) ; true ),
	(   var(Pfile)
	->  true % initial load file not specified
	;   (   ( atom(Pfile), exists_file(Pfile) )
	    ->	dpl:load_decl_policy(Pfile,PolicyName),
		pap:set_current_policy(PolicyName),
		format('policy ~q loaded from ~a~n',[PolicyName,Pfile]),
		audit_gen(policy_admin, importopt(Pfile,PolicyName,success))
	    ;   format('policy file load error: ~a~n',[Pfile]),
		audit_gen(policy_admin, importopt(Pfile,failure))
	    )
	),

        (   param:epp_status(policy_server)
	->  epp:epp_with_server
	;   true
	),

	param:server_sleeptime(S), go_to_sleep(S),
	true.

go_to_sleep(S) :-
	sleep(S),
	periodic_goals,
	go_to_sleep(S).

periodic_goals :-
	% add periodic Policy Server goals here
	true.

create_server_audit_log :- param:audit_logging(file), !,
	audit:gen_time_stamp(TS),
	atomic_list_concat(['LOG/audit_log','_',TS],LogFile),
	format('Audit log file: ~w~n',LogFile),
	open(LogFile,append,AudStream),
	param:setparam(audit_stream,AudStream).
create_server_audit_log.


