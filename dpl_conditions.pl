% Conditions used in policy evaluation by the PDP

:- module(dpl_conditions,[c_assign/3,c_associate/4, built_in_binary_relation_name/1,
                          request_conditions/1, evaluate_condition/1]).

:- use_module(dpl,[assign/3,associate/4,cond/3,conditions/2]).

%:- dynamic condition_variables/1, condition_predicates/1.
%:- discontiguous condition_variable/2.
%:- multifile condition_variable/2.


% ------------------------------------------------------------------------
% INITIALIZE CONDITION VARIABLES AND PREDICATES
%

:- dynamic dpl_conditions_initialized/1.

dpl_conditions_initialized(false).

init :-
    %predefined_condition_predicates(PCPs), assert( defined_condition_predicates(PCPs) ),
    init_predefined_condition_predicates,
    init_predefined_condition_variables,
    (   exists_file('conditions.pl')
    ->  ensure_loaded(conditions)
    ;   true
    ),
    install_condition_variables,
    install_condition_predicates,
    retractall( dpl_conditions_initialized(_) ), assert( dpl_conditions_initialized(true) ).

re_init :- un_init, init.

un_init :-
    clear_conditions,
    retractall( dpl_conditions_initialized(_) ), assert( dpl_conditions_initialized(false) ).

clear_conditions :-
    retractall( condition_predicate(_,_) ), retractall( condition_variable(_) ),
    retractall(defined_condition_variables(_)), assert(defined_condition_variables([])),
    retractall( defined_condition_predicates(_) ), assert( defined_condition_predicates([]) ),
    retractall( requested_condition_predicates(_) ), assert( requested_condition_predicates([]) ),
    true.


% ------------------------------------------------------------------------
%
% CONDITIONAL DPL ELEMENTS
%
%    c_assign and c_associate are referenced by PDP
%    these, in turn, reference assign and associate in DPL

c_assign(Policy,E1,E2) :-
    assign(Policy,E1,E2),
    (   cond(Policy, ConditionExpr, assign(E1,E2))
    ->  evaluate_condition(ConditionExpr)
    ;   true
    ).

c_associate(Policy,UA,OpSet,OA) :- % hook for conditional associations
    associate(Policy,UA,OpSet,OA),
    (   cond(Policy, ConditionExpr, associate(UA,OpSet,OA))
    ->  evaluate_condition(ConditionExpr)
    ;   true
    ).

% ------------------------------------------------------------------------
%
% CONDITION EXPRESSIONS
%
%     <condition expression> ::= <boolean constant>
%         | <boolean condition variable>
%         | <condition predicate>
%         | <built-in relation>
%
%     <condition predicate> ::= <condition predicate name>
%         | <condition predicate name>(<condition arguments>)
%
%     <condition arguments> ::= <condition argument>
%         | <condition arguments> , <condition argument>
%
%     <condition argument> ::= <condition variable> | <scalar constant>
%         | <list constant>
%
%     <scalar constant> ::= <boolean constant> | <number> | <name>
%
%     <boolean constant> ::= true | 'True' | false | 'False'
%
%     <list constant> ::= [ <constant list items> ]
%
%     <constant list items> ::= <constant>
%         | <constant list items> , <constant>
%
%     <built-in relation> ::=
%         <built-in relation name>( <relation arg> , <relation arg> )
%
%     <built-in relation name> :==
%         is_equal_to | is_unequal_to | is_member_of
%         | is_less_than | is_greater_than
%         | is_less_than_or_equal_to | is_greater_than_or equal_to
%
%  Condition variables are referenced using
%     condition_variable_value/2

evaluate_condition(C) :- is_True(C), !. % constant true
evaluate_condition(C) :- is_False(C), !, fail. % constant false
evaluate_condition(C) :- validate_condition_variable(C,boolean), !,
    condition_variable_value(C,V), V == true.
evaluate_condition(C) :-
    validate_built_in_binary_relation(C), !,
    substitute_args(C,S,2),
    evaluate_relation(S).
evaluate_condition(C) :-
    validate_condition_predicate(C,N), !,
    substitute_args(C,CS,N),
    evaluate_condition_predicate(CS).

evaluate_condition_predicate(P) :-
    % may need something more sophisticated here
    % e.g. should probably check argument types
    call(P).

evaluate_relation(is_equal_to(X,Y)) :- X==Y, !.
evaluate_relation(is_unequal_to(X,Y)) :- X\==Y, !.
evaluate_relation(is_member_of(E,L)) :-
    (   validate_condition_variable(E)
    ->  condition_variable_value(E,ElementVal)
    ;   ElementVal=E
    ),
    (   validate_condition_variable(L)
    ->  condition_variable_value(L,ListVal)
    ;   ListVal=L
    ), is_list(ListVal),
    memberchk(ElementVal,ListVal), !.
evaluate_relation(is_subset_of(L1,L2)) :-
    (   validate_condition_variable(L1)
    ->  condition_variable_value(L1,ListVal1)
    ;   ListVal1=L1
    ),
    (   validate_condition_variable(L2)
    ->  condition_variable_value(L2,ListVal2)
    ;   ListVal2=L2
    ), is_list(ListVal1), is_list(ListVal2),
    subset(ListVal1,ListVal2), !.
evaluate_relation(is_less_than(X,Y)) :- X < Y, !.
evaluate_relation(is_greater_than(X,Y)) :- X > Y, !.
evaluate_relation(is_less_than_or_equal_to(X,Y)) :- X =< Y, !.
evaluate_relation(is_greater_than_or_equal_to(X,Y)) :- X >= Y, !.

% ------------------------------------------------------------------------
% Substitute values into condition arguments
%
substitute_args(T,T,0) :- !.
substitute_args(T1,T2,N) :-
    functor(T1,F,N), functor(T2,F,N),
    T1 =.. [F|Args1], T2 =.. [F|Args2],
    substitute_args(Args1,Args2),
    true.

substitute_args([],[]).
substitute_args([A1|Args1],[A2|Args2]) :-
    substitute_arg(A1,A2),
    substitute_args(Args1,Args2).

substitute_arg(A1,A2) :- condition_variable(A1:_T1), !,
    condition_variable_value(A1,A2).
substitute_arg(A,A) :- validate_constant(A), !.
% may need additional clauses for other types of args
%


% ------------------------------------------------------------------------
% Validate elements of conditions
%
validate_built_in_binary_relation( R ) :- R =.. [RN,X,Y],
    built_in_binary_relation_name(RN),
    (   validate_constant(X) ; validate_condition_variable(X)  ),
    (   validate_constant(Y) ; validate_condition_variable(Y)  ),
    !.

built_in_binary_relation_name(N) :-
    built_in_binary_relations(Rs), R=..[N,_,_],
    memberchk(R,Rs).

built_in_binary_relations([is_equal_to(any,any),
                    is_unequal_to(any,any),
                    is_member_of(any,list),
                    is_subset_of(list,list),
                    is_less_than(number,number),
                    is_greater_than(number,number),
                    is_less_than_or_equal_to(number,number),
                    is_greater_than_or_equel_to(number,number)
                   ]).


validate_constant(X) :- boolean_constant(X), !.
validate_constant(X) :- atom(X), !.
validate_constant(X) :- is_list(X), ground(X), !.

boolean_constant(X) :- (is_True(X) ; is_False(X)), !.

validate_condition_variable(V) :- validate_condition_variable(V,_), !.
validate_condition_variable(V,T) :- atom(V),
    defined_condition_variables(CVs), memberchk(V:T,CVs).

validate_condition_predicate(P,_Nargs) :- atom(P), !,
    defined_condition_predicates(CPs),
    compound_name_arity(GP,P,0),
    memberchk(GP,CPs).
validate_condition_predicate(P,Nargs) :-
    compound_name_arguments(P,PN,Pargs), % P =.. [PN|Pargs],
    length(Pargs,Nargs),
    compound_name_arity(GP,PN,Nargs),
    compound_name_arguments(GP,PN,Gargs), % GP =.. [PN|Gargs],
    % maybe could just do: condition_predicate(PN,Gargs)
    defined_condition_predicates(CPs),
    memberchk(GP,CPs),
    check_pred_args(Pargs,Gargs).

check_pred_args([],[]).
check_pred_args([PA|PAs],[GA|GAs]) :-
    check_pred_arg(PA,GA),
    check_pred_args(PAs,GAs).

check_pred_arg(P,list) :- is_list(P), !.
check_pred_arg(P,boolean) :- (P==true ; P==false ; P=='True' ; P=='False'), !.
check_pred_arg(P,number) :- number(P), !.
check_pred_arg(P,name) :- atom(P), !.
check_pred_arg(P,any) :- ground(P), !.


% ------------------------------------------------------------------------
% ------------------------------------------------------------------------
% CONDITION PREDICATE DEFINITIONS

:- dynamic condition_predicate/2.
:- multifile condition_predicate/2.

%predefined_condition_predicates([]).

% if there are predefined condition predicates, place definitions below
%   e.g.: condition_predicate(PredicateName, ListOfPredArgTypes).
predefined_condition_predicate(unix, []).
predefined_condition_predicate(windows, []).
predefined_condition_predicate(is_True, [boolean]).
predefined_condition_predicate(is_False, [boolean]).
% ...

% definitions of predefined condition predicates
%
unix :- current_prolog_flag(unix,true).
windows :- current_prolog_flag(windows,true).
is_True(X) :- atom(X), (X==true;X=='True'), !.
is_False(X) :- atom(X), (X==false;X=='False'), !.

init_predefined_condition_predicates :- % assumes clean init or re-init
    forall( predefined_condition_predicate(N,A), assert( condition_predicate(N,A) ) ).

%:- dynamic condition_predicates/1.
:- dynamic defined_condition_predicates/1, requested_condition_predicates/1.

defined_condition_predicates([]).

requested_condition_predicates([]).

%condition_predicates([current_day_is_one_of(list)]).


%current_day_is_one_of(SetOfDays) :-
%    condition_variable_value(today,Today),
%    memberchk(Today,SetOfDays).


% ------------------------------------------------------------------------
% This is called by dpl to request conditions needed for a policy.
% Can expand on this later to do availability checks or deferred
% availability.

request_conditions(Conditions) :-
    check_requested_conditions(Conditions),
    assert(requested_condition_predicates(Conditions)).

%request_conditions([]).
%request_conditions([Condition|Conditions]) :-
%    request_condition(Condition),
%    request_conditions(Conditions).
%request_condition(Condition).

check_requested_conditions(_). % detail later, check syntax, etc.


% ------------------------------------------------------------------------
%

install_condition_predicates :- % assumes condition_predicate already exists for predefined predicates
    defined_condition_predicates(CurrentPredicates),
    findall(Predicate,
            (   condition_predicate(PredName, PredArgs), compound_name_arguments(Predicate,PredName,PredArgs) ),
            Predicates),
    install_condition_predicates(Predicates),
    append(CurrentPredicates,Predicates,AllPredicates),
    retractall(defined_condition_predicates(_)),
    assert(defined_condition_predicates(AllPredicates)).

install_condition_predicates([]).
install_condition_predicates([Condition|Conditions]) :-
    install_condition(Condition),
    install_condition_predicates(Conditions).

install_condition(_C) :- % for now, do nothing more
    true.


% ------------------------------------------------------------------------
% ------------------------------------------------------------------------
% CONDITION VARIABLE DEFINITIONS
%
%   condition_variable(VariableName : VariableType)
%
%   condition_variable(zero:number).
%   condition_variable(today:name).
%
%   condition_variables([zero:number,today:name]).

:- dynamic condition_variable/1.
:- multifile condition_variable/1.

:- dynamic condition_variables/1.
:- multifile condition_variables/1.

% predefined condition variables below
%   e.g.: condition_variable(VariableName : VariableType).
predefined_condition_variable(zero:number).
% predefined_condition_variable(today:name). % this one mapped to a context variable
predefined_condition_variable(local_day:name). % these 3 are from time of local host
predefined_condition_variable(local_hour:number).
predefined_condition_variable(local_minute:number).
% ...


init_predefined_condition_variables :- % assumes clean init or re-init
    forall( predefined_condition_variable(V), assert( condition_variable(V) ) ).

%:- dynamic condition_variables/1.
%:- multifile condition_variables/1.

% condition_variables([zero:number,local_day:name,local_hour:number,local_minute:number]).
% % pre-defined

:- dynamic defined_condition_variables/1.

defined_condition_variables([]).


% ------------------------------------------------------------------------


install_condition_variables :-
    findall(CondVar1, condition_variable(CondVar1), CondVars1),
    findall(CondVar2, (condition_variables(Vars), member(CondVar2,Vars)), CondVars2),
    append(CondVars1,CondVars2,CondVars),
    install_condition_variables(CondVars),
    retractall(defined_condition_variables(_)),
    assert(defined_condition_variables(CondVars)).

    %findall(VarName:VarType, condition_variable(VarName:VarType), Variables),
    %append(CurrentVariables,Variables,AllVariables),
    %retractall(condition_variables(_)),
    %assert(condition_variables(AllVariables)).

install_condition_variables(_) :- !. % do nothing for now
install_condition_variables([]).
install_condition_variables([Variable|Variables]) :-
    install_condition_variable(Variable),
    install_condition_variables(Variables).

install_condition_variable(_V). % do nothing for now

% ------------------------------------------------------------------------
% CONDITION VARIABLE VALUES
%
%   Policy Server DPL to EPP_CPA INTERFACE for condition variables
%   mapped to context variables
%
%   Perform condition to context mapping when reading a condition
%   variable. Uses epp_cpa:read_context_cache/2 when the referenced
%   condition variable is mapped to a context variable

:- use_module(epp_cpa,[condition_context_variable_map/2, read_context_cache/2]).

condition_variable_value(CondVar,Val) :-
    condition_variable(CondVar:Type), !,
    (   condition_context_variable_map(CondVar,CtxVar)
    ->  read_context_cache(CtxVar:Type,Val)
    ;   read_local_condition_variable(CondVar,Val)
    ).
condition_variable_value(_,undefined).

% ------------------------------------------------------------------------

read_local_condition_variable(Name,Val) :- atom(Name), var(Val), !,
    local_condition_variable_value(Name,Val).

% define predicates to return local condition variable values below at
% ...
%
local_condition_variable_value(zero,0) :- !.
local_condition_variable_value(local_time,Time) :- !, % this may not be useful for comparisons
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,Hour,Min,Sec,_,_,_),
    format(atom(Time), '~d:~d:~d ~d:~d:~d',[Year,Month,Date,Hour,Min,Sec]).
local_condition_variable_value(local_day,DayOfWeek) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,_Hour,_Min,_Sec,_,_,_),
    day_of_the_week(date(Year,Month,Date),Day),
    nth1(Day,['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'],DayOfWeek).
local_condition_variable_value(local_hour,Hour) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(_Year,_Month,_Date,Hour,_Min,_Sec,_,_,_).
local_condition_variable_value(local_minute,Min) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(_Year,_Month,_Date,_Hour,Min,_Sec,_,_,_).
% ...
local_condition_variable_value(_,undefined).

