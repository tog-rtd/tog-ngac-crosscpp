% Policy Information Point
% Work in Progress
% must bring up to current version of module dpl

:- module(pip,[init/0,
	        policy/2, element/2, assign/3, associate/4,
		load_decl_policy/2, save_decl_policy/2,
		object_oattribute/2, object/2, object/8,
		user_uattribute/2, user/2
	       ]).

:- use_module(policies).
:- use_module(pio).
:- use_module(param).

% :- include('TEST/pip_test').

% The lightweight policy model
%
% Cache of loaded policies as asserted clauses
% policy(PolicyName, PolicyRoot)
%
% element(PolicyName:PolicyRoot, Element)
%   where Element is: user(_), user_attribute(_)
%     object(_), object_attribute(_), policy_class(_),
%     opset(OpSetName, OpList), operation(OpName, OpInfo), operation(OpName),
%     connector('pm') is an element of every policy
%     assign(PolicyName:PolicyRoot, PolicyElement1, PolicyElement2)
%     associate(PolicyName:PolicyRoot, UserAttr, OpSetName, ObjectAttr)

:- dynamic policy/2, element/2, assign/3, associate/4.
% policy(PolicyName, PolicyClass)
%

% Work in Progress
% must bring up to current version of module dpl

policy_elements([user,user_attribute,object,object_attribute,policy_class,
		 operation,opset,composed_policy,assign,associate,connector,cond]).

policy_elements_args([user(_),user_attribute(_),
		      object(_),object(_,_,_,_,_,_,_),
		      object_attribute(_),policy_class(_),operation(_),operation(_,_),
		      opset(_,_),composed_policy(_,_,_),assign(_,_),associate(_,_,_),
		      connector(_)],cond(_,_)).

conditional_policy_elements_args([associate(_,_,_)]).

init:- param:initialized(true), !. % already initialized
init :-
	forall( policies:policy(Pn,Pr,Pg), unpack_policy( policy(Pn,Pr,Pg) ) ),
	param:setparam(initialized,true),
	true.

re_init :- un_init, init.

un_init :-
	clear_policy,
	param:setparam(initialized,false).

clear_policy :-
	retractall(policy(_,_)), retractall(element(_,_)),
	retractall(assign(_,_,_)), retractall(associate(_,_,_,_)),
	param:setparam(current_policy,none),
	true.
