:- module(pio,[policy_cmdstrs/2,
	       save_cmdstrs_to_file/2,
	       display_policy/1, graph_policy/1,
	       display_conditions/0, display_conditions/1
	      ]).
% Input / Output of various policy representations

:- use_module([dpl, dpl_conditions]).


policy_cmdstrs(P,CmdStrs) :- % version 2
	findall( add(p(PC), c('PM')),
		 element(P,policy_class(PC)),
		 PCs ),

	findall( add(b(OA), p(PC)),
		 ( element(P,object_attribute(OA)), element(P,policy_class(PC)), assign(P,OA,PC) ),
		 OAs1 ),

	findall( add(a(UA), p(PC)),
		 ( element(P,user_attribute(UA)), element(P,policy_class(PC)), assign(P,UA,PC) ),
		 UAs1 ),

	findall( add(a(UA1), a(UA2)),
		 ( element(P,user_attribute(UA1)), element(P,user_attribute(UA2)), assign(P,UA1,UA2) ),
		 UAs2 ),

	findall( add(u(U), a(UA)),
		 ( element(P,user(U)), element(P,user_attribute(UA)), assign(P,U,UA) ),
		 Us ),

	%OC = 'File', Host = 'Host', Path = 'Path', % need to fix this
	findall( add(ob(O,OC,Inh,Host,Path,BType,OA)),
		 ( object(P,O,OC,Inh,Host,Path,BT,OA), type_map(BT,BType), element(P,object_attribute(OA)), assign(P,O,OA) ),
		 Os ),

	findall( [add(s(OPsetName),oc(ignored),a(UA)), AddOps, asg(s(OPsetName),b(OA))],
		 ( c_associate(P,UA,OPset,OA),
		   atomic_list_concat(['{',UA,'-[]-',OA,'}'],'',OPsetName),
		   findall(add(op(OP),s(OPsetName)), member(OP,OPset), AddOps) ),
		 NAssocs ), flatten(NAssocs,Assocs),

	append([PCs,UAs1,OAs1,UAs2,Us,Os,Assocs], CmdTerms),
	pmcmd:pmCmdStrs_CmdTerms(CmdStrs, CmdTerms),
	true.

type_map(object_attribute,b).

% load/save a single Prolog term from/to a file
load_term(FileName, Term) :- exists_file(FileName), !,
	open(FileName, read, File),
	read_term(File, Term, []),
	close(File).
load_term(FileName, _) :-
	ui:notify(FileName,'Does not exist.').

save_term(FileName, Term) :-
	open(FileName, write, File),
	writeq(File, Term), write(File,'.\n'),
	close(File), !.

save_cmdstrs_to_file(FileName, CmdStrs) :-
	open(FileName, write, File),
	save_cmdstrs(File, CmdStrs),
	close(File), !.

save_cmdstrs(_,[]).
save_cmdstrs(F,[Cmd|Cmds]) :-
	write(F,Cmd), nl(F),
	save_cmdstrs(F,Cmds).

%%

save_cmdterms_to_file(FileName, CmdTerms) :-
	open(FileName, write, File), write(File,'[\n'),
	save_cmdterms(File, CmdTerms), write(File, '].\n'),
	close(File), !.

save_cmdterms(_,[]).
save_cmdterms(F,[Term|Terms]) :-
	writeq(F,Term),
	(   Terms \== []
	->  write(F,',')
	;   true
	), nl(F),
	save_cmdterms(F,Terms).

% load a list of command terms [ term, term, ... ].
load_cmdterms_from_file(FileName, CmdTerms) :-
	load_term(FileName, CmdTerms).

% Reading and writing a PM state file
%
%   The PM state is a list of atoms that are each a CmdStr.
%   The PM state file is one CmdStr per line.
%

load_CmdTerms_from_CmdStrFile(F,CmdTerms) :-
	load_CmdStrs_from_file(F,CmdStrs),
	pmcmd:pmCmdStrs_CmdTerms(CmdStrs, CmdTerms).

% load_CmdStrs_from_file returns a list of commands from the file
%
load_CmdStrs_from_file(F,CmdStrs) :- exists_file(F), !,
	open(F,read,FS),
	load_CmdStrs_from_stream(FS,CmdStrs),
	close(FS).
load_CmdStrs_from_file(F, _) :-
	ui:notify(F,'Does not exist.').

% load_CmdStrs_from_stream returns a list of commands from a stream
%
% load_CmdStrs_from_stream(Stream,CmdStrs) :-
%	% need to deal with comment lines (beginning with #)
%	read_string(Stream, "\n", "\r\t ", End, S),
%	(   End \== -1
%	->  atom_codes(Sa,S),
%	    CmdStrs = [Sa|Ss],
%	    load_CmdStrs_from_stream(Stream,Ss)
%	;   CmdStrs = []
%	).

load_CmdStrs_from_stream(Stream,CmdStrs) :-
	read_string(Stream, "\n", "\r\t ", End, S),
	(   End \== -1
	->  ( % comment-out following 5 lines to keep comments/blank lines
%		( sub_string(S,0,1,_,"#") ; string_length(S,0) )
%	        % throw away comment and blank lines
%	    ->
%		load_CmdStrs_from_stream(Stream,CmdStrs)
%	    ;
		atom_codes(Sa,S),
		CmdStrs = [Sa|Ss],
		load_CmdStrs_from_stream(Stream,Ss)
	    )
	;   CmdStrs = []
	).

% pmCmdTerms_to_file converts a list of command terms to a list
% of command strings and writes them to a file
%
save_CmdTerms_to_CmdStrFile(F,CmdTerms) :-
	maplist(pmcmd:pmCmdStr_CmdTerm, CmdStrs, CmdTerms),
	save_CmdStrs_to_file(F,CmdStrs).

% pmCmdStrs_to_file writes a list of commands to a file
%
save_CmdStrs_to_file(F,CmdStrs) :-
	open(F,write,FS),
	save_CmdStrs_to_stream(FS,CmdStrs),
	close(FS).

% pmCmdStrs_to_stream writes a list of commands to a stream
%
save_CmdStrs_to_stream(_,[]) :- !.
save_CmdStrs_to_stream(Stream,[S|Ss]) :-
	write(Stream,S), nl(Stream),
	save_CmdStrs_to_stream(Stream,Ss).

% DISPLAY CONDITIONS
display_conditions :- display_conditions(all).

display_conditions(all) :- !,
	forall( dpl_conditions:cond_name(Cname,_), display_conditions(Cname) ).
display_conditions(Cname) :- dpl_conditions:is_cond_name(Cname), !,
	format('conditions(~q):~n',[Cname]),
	forall( dpl_conditions:condition_variable(V:T,Cname), format('  condition_variable(~q).~n',[V:T]) ),
	forall( ( dpl_conditions:condition_predicate(Pred,Cname,_), compound_name_arguments(Pred,Pname,Pargs) ),
		format('  condition_predicate(~q,~q).~n',[Pname,Pargs]) ),
	forall( ( dpl_conditions:condition_predicate(Pred,Cname,defined), compound_name_arity(Pred,Pname,_) ),
		listing(dpl_conditions:Pname) ).
display_conditions(_). % silently succeeds if argument is unknown

% DISPLAY POLICY
display_policy(P) :- policy(P,PC), !,
	format('policy(~q, ~q, [~n', [P,PC]),
	(   conditions(P:PC,Conditions)
	->  format('  conditions(~q),~n',Conditions)
	;   true
	),
	forall(element(P:PC,user(U)), format('  user(~q),~n',U)),
	forall(element(P:PC,user_attribute(UA)), format('  user_attribute(~q),~n',UA)),
	forall(element(P:PC,object(O)), format('  object(~q),~n',O)),
	forall(element(P:PC,object_attribute(OA)), format('  object_attribute(~q),~n',OA)),
	forall(element(P:PC,policy_class(PC)), format('  policy_class(~q),~n',PC)),
	forall(element(P:PC,connector(C)), format('  connector(~q)',C)),
	forall(assign(P:PC,E1,E2), format(',~n  assign(~q,~q)',[E1,E2])),
	forall(associate(P:PC,E1,Ops,E2), format(',~n  associate(~q,~q,~q)',[E1,Ops,E2])),
	forall(cond(P:PC,Cond,E), format(',~n  cond(~q, ~q)',[Cond,E])),
	format('~n]).~n').

% GRAPH POLICY
graph_policy(P) :- policy(P,PC), !,
	(   conditions(P:PC,_)
	->  format('# Conditional policies not currently supported. Conditions ignored.')
	;   true
	),
	findall(U, element(P:PC,user(U)), Users),
	findall(O, element(P:PC,object(O)), Objects),
	findall(UA, element(P:PC,user_attribute(UA)), UAs),
	findall(OA, element(P:PC,object_attribute(OA)), OAs),
	findall(APC, element(P:PC,policy_class(APC)), PCs),
	format('~w~w~w~n~w~n~w~n',
	       [
		   'strict digraph "', P, '" {',
		   '  node [shape=none]',
		   '  APC [label="",width=0,height=0];'
	       ]),
	(   ( Users \== [], UAs \== [] )
	->  format('~w~n~w~n~w~n~w~n',
	       [
		   '  "<Users>"->"<User Attributes>" [arrowhead=none,style=invis];',
		   '  "<User Attributes>"->APC [arrowhead=none,style=invis];',
		   '  APC->"<Policy Classes>" [arrowhead=none,style=invis];',
		   '  node [shape=ellipse]'
	       ])
	;   format('  node [shape=ellipse]~n')
%	;   format('~w~n',
%	       [
%		   ' ""->"<User Attributes>" [arrowhead=none,style=invis];'
%	       ])

	),

%	(   UAs \== [] % unfinished
%	->  format('~w~n',
%	       [
%		   ' "<User Attributes>"->APC [arrowhead=none,style=invis];'
%	       ])
%	;   format('~w~n',
%	       [
%		   ' "<User Attributes>"->APC [arrowhead=none,style=invis];'
%	       ])
%
%	),

%	format('~w~n~w~n',
%	       [
%		   ' APC->"<Policy Classes>" [arrowhead=none,style=invis];',
%		   '  node [shape=ellipse]'
%	       ]),


	forall(member(U,Users), (dq(U,Uq), format('  ~w [peripheries=2];',Uq))), nl,
	forall(member(O,Objects), (dq(O,Oq), format('  ~w;',Oq))),
	forall((assign(P:PC,E1,E2),element(P:PC,user(E1))),
	       (dq(E1,E1q), dq(E2,E2q), format('~n  ~w -> ~w;',[E1q,E2q]))),
	forall((assign(P:PC,E1,E2),element(P:PC,object(E1))),
	       (dq(E1,E1q), dq(E2,E2q), format('~n  ~w -> ~w;',[E1q,E2q]))),
	forall((assign(P:PC,E1,E2),\+element(P:PC,user(E1)),\+element(P:PC,object(E1))),
	       (dq(E1,E1q), dq(E2,E2q), format('~n  ~w -> ~w;',[E1q,E2q]))),

	forall(associate(P:PC,E1,Ops,E2), % COME BACK AND TAKE CARE OF OPS <<<===
	       (dq(E1,E1q), dq(E2,E2q), maplist(dq, Ops, Opsq),
		format('~n  ~w -> ~w [constraint=false,label="~w",style=dashed,arrowhead=none];',[E1q,E2q,Opsq]))),

	format('~n  { rank=same; '),
	forall(( member(E,Users); member(E,Objects) ), (dq(E,Eq), format('~w ',Eq))),
	format('}'),

	format('~n  { rank=same; '),
	forall( unconflicted_first_level(P:PC, A), (dq(A,Aq), format('~w ',Aq))),
	format('}'),

	format('~n  { rank=same; '),
	forall( last_level(P:PC, A), (dq(A,Aq), format('~w ',Aq))),
	format('}'),
%
%
	(   Users \== []
	->  format('~n~w~n~w',
	       [
		   '  subgraph user_dag {',
		   '    subgraph u_nodes {'
	       ]),
	    format('~n      '),
	    forall(member(U,Users), (dq(U,Uq), format('~w; ',Uq))),
	    maplist(dq,Users,Usersq),
	    atomic_list_concat(Usersq,'->',Uchain),
	    format('~n      '),
	    format('~w [style=invis];',Uchain),
	    format('~n    }')
	;   format('~n~w',
	       [
		   '  subgraph user_dag {'
	       ])
	),
%	format('~n~w~n~w',
%
	format('~n~w',
	       [
		   '    subgraph ua_nodes {'
	       ]),
	format('~n      '),
	forall(member(UA,UAs), (dq(UA,UAq), format('~w; ',UAq))),
	format('~n~w~n~w',
	       [
		   '    }',
		   '  }'
	       ]),
%
%
	(   Objects \== []
	->  format('~n~w~n~w',
	       [
		   '  subgraph object_dag {',
		   '    subgraph o_nodes {'
	       ]),
	    format('~n      '),
	    forall(member(O,Objects), (dq(O,Oq), format('~w; ',Oq))),
	    maplist(dq,Objects,Objectsq),
	    atomic_list_concat(Objectsq,'->',Ochain),
	    format('~n      '),
	    format('~w [style=invis];',Ochain),
	    format('~n    }')
	;   format('~n~w',
	       [
		   '  subgraph object_dag {'
	       ])
	),
	format('~n~w',
	       [
		   '    subgraph oa_nodes {'
	       ]),
%
%
	format('~n      '),
	forall(member(OA,OAs), (dq(OA,OAq), format('~w; ',OAq))),
	format('~n~w~n~w~n~w',
	       [
		   '    }',
		   '  }',
		   '  subgraph pc_nodes {'
	       ]),
	forall(member(APC,PCs), (dq(APC,APCq), format('~n    ~w [shape=polygon,sides=5];',APCq))),
	format('~n  }'),
%	(   (Objects \== []; OAs \== [])
	(   Objects \== []
	->  format('~n~w~n~w',
	       [
		   '  node [shape=none]',
		   '  "<Objects>"->"<Object Attributes>" [arrowhead=none,style=invis]'
	       ])
	;   format('~n~w~n~w',
	       [
		   '  node [shape=none]',
		   '  "<Object Attributes>"'
	       ])
	),
	format('~n}~n').

unconflicted_first_level(P:PC,A) :-
	% unconflicted here means that the attribute has an assigned user or object
	% but is not itself directly assigned to a policy class
	( element(P:PC, user(E)) ; element(P:PC, object(E) ) ),
	assign(P:PC, E, A),
	\+ ( element(P:PC, policy_class(APC)), assign(P:PC, A, APC) ).

last_level(P:PC,A) :-
	( element(P:PC, user_attribute(A)) ; element(P:PC, object_attribute(A) ) ),
	element(P:PC, policy_class(APC)), assign(P:PC, A, APC).

dq(A,QA) :- % may be some other cases to take care of, e.g. 'f(x)'
	write_length(A,Al,[]), write_length(A,Alq,[quoted(true)]),
	%format('write_length ~d; write_length quoted ~d~n',[Al,Alq]),
	(   Al =\= Alq
	->  (	special_chk(A) % sub_atom(A,_,_,_,' ')
	    ->	format(atom(QA),'"~w"',A)
	    ;	format(atom(QA),'~w',A)
	    )
	;   format(atom(QA),'~w',A)
	),
	!, %writeln(QA),
	true.

special_chk(X) :- atom(X),
	atom_chars(X,Xs), atom_chars(' .,;()-!@#$%^&*+=[]{}|\\/<>?', Special),
	\+ intersection(Xs, Special, []).
