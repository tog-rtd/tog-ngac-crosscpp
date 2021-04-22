% Conditions used in policy evaluation by the PDP

:- module(dpl_conditions,[c_assign/3, c_associate/4, c_associate/5, built_in_binary_relation_name/1,
                          request_conditions/1, evaluate_condition/1, evaluate_condition_with_args/2,
                          add_cond_elements/2, delete_cond_elements/2,
                          conditions_reset/0, conditions_reset/1]).

:- use_module(dpl,[assign/3,associate/4,cond/3,conditions/2]).
:- use_module(ui). % because condition predicates may use timestamp conversions

%:- discontiguous condition_variable/2.
%:- multifile condition_variable/2.


% ------------------------------------------------------------------------
% INITIALIZE CONDITION VARIABLES AND PREDICATES
%

:- dynamic dpl_conditions_initialized/1.

dpl_conditions_initialized(false).

init :-
    init_predefined_condition_variables,
    init_predefined_condition_predicates,
    (   exists_file('conditions.pl')
    ->  ensure_loaded(conditions)
    ;   true
    ),
    init_static_condition_variables,
    init_static_condition_predicates,
    retractall( dpl_conditions_initialized(_) ), assert( dpl_conditions_initialized(true) ),
    !.

re_init :- un_init. %, init.

un_init :-
    %clear_conditions,
    conditions_reset(all),
    retractall( dpl_conditions_initialized(_) ), assert( dpl_conditions_initialized(false) ).

% CLEAR CONDITIONS
% this series currently not used - see conditions_reset

clear_conditions :- % TODO must add clearing of condition predicate definitions
    clear_condition_predicates,
    retractall( condition_variable(_) ),
    retractall( declared_condition_variables(_) ), assert( declared_condition_variables([]) ),
    retractall( declared_condition_predicates(_) ), assert( declared_condition_predicates([]) ),
    retractall( requested_condition_predicates(_) ), assert( requested_condition_predicates([]) ),
    true.

clear_condition_predicates :-
    findall((Pname,Pargs), ( condition_predicate(Pname,Pargs), \+predefined_condition_predicate(Pname,_) ), CondPreds),
    forall( member((Pname,Pargs),CondPreds),
            (clear_condition_predicate_decl(Pname,Pargs), clear_condition_predicate_def(Pname,Pargs) ) ).

clear_condition_predicates(_CName) :-
    true.

clear_condition_predicate_decl(Pname,_Pargs) :- retractall( condition_predicate(Pname, _) ).

clear_condition_predicate_def(Pname,Pargs) :-
    is_list(Pargs), length(Pargs,Nargs), functor(Head,Pname,Nargs),
    (   clause(Head,_)
    ->  retractall( Head )
    ;   true
    ).

% CONDITIONS RESET
%
% conditions_reset/1 called from PAP
% this approach does not utilize the lists of variables and predicates,
% just condition_variable/2 and condition_predicate/3 clauses

conditions_reset :- conditions_reset(all).
conditions_reset(all) :-  !, % 'all' does not include predefined and static
    conditions_reset(dynamic),
    forall( ( dpl_conditions:cond_name(Cname,_), \+is_special_cond_name(Cname) ), conditions_reset(Cname) ).
conditions_reset(predefined) :- !. % silently ignore predefined and static
conditions_reset(static) :- !.
conditions_reset(Cname) :- dpl_conditions:is_cond_name(Cname), !,
    forall( condition_variable(V:T,Cname), retractall( condition_variable(V:T,Cname) ) ),

    forall( ( condition_predicate(Pred,Cname,defined),
              compound_name_arity(Pred,Pname,N), functor(PT,Pname,N) ),
            retractall(dpl_conditions:PT) ),

    forall(condition_predicate(Pred,Cname,_), retractall(condition_predicate(Pred,Cname,_))),
    (   \+is_special_cond_name(Cname)
    ->  retractall( cond_name(Cname,_) )
    ;   true
    ).

% ADD CONDITION ELEMENTS
%   dynamic add, called from pap

add_cond_elements(predefined,_) :- fail. % cannot dynamically add to predefined
add_cond_elements(static,_) :- fail. % cannot dynamically add to static
add_cond_elements(dynamic,CElements) :- !, % unnamed condition dynamic add
    add_cond_elements1(dynamic,CElements).
add_cond_elements(Cname,CElements) :- is_cond_name(Cname,OldElements), !, % named already exists
    % alternative would be to fail here for attempt to redefine a named list
    % !, fail,
    % delete already existing conditions associated with name
    retractall( cond_name(Cname,_) ),
    delete_cond_elements(Cname,OldElements),
    add_cond_elements1(Cname,CElements),
    assert( cond_name(Cname,CElements) ).
add_cond_elements(Cname,CElements) :- atom(Cname), !, % new name
    add_cond_elements1(Cname,CElements),
    assert( cond_name(Cname,CElements) ).

add_cond_elements1(_,[]).
add_cond_elements1(Cname,[Elt|EltList]) :-
    add_cond_element(Cname,Elt),
    add_cond_elements1(Cname,EltList).

add_cond_element( Cname, condition_variable(V) ) :- !,
    dynamic_install_condition_variable(Cname,V).
add_cond_element( Cname, condition_predicate(Pname,Pargs) ) :- !,
    % e.g. condition_predicate(current_day_is_one_of, [list])
    % predicate will be dynamic if not already declared as a static pred which will fail
    % should probably check for no conflict before attempting to assert in following clause
    dynamic_install_condition_predicate(Cname,Pname,Pargs).
add_cond_element( Cname, Clause) :- atom(Cname), Clause=(H:-_B), !, % a condition predicate definition
    % predicate will be dynamic if not already declared as a static pred which will fail
    % must still be installed so that it can be uninstalled or cleared by clear_conditions
    % do not assert again if already there
    % check that there is a declaration for the predicate
    (   clause(H,_) % leave it alone if already defined
    ->  true
    ;   assertz(Clause)
    ),
    compound_name_arity(H,Pname,N), functor(P,Pname,N),
    (   condition_predicate(P, Cname, PDef)
    ->  (   PDef==undefined
        ->  retractall( condition_predicate(P,Cname,PDef) ),
            assert( condition_predicate(P,Cname,defined) )
        ;   true
        )
    ;   assert( condition_predicate(H, Cname, defined) ) % not a good thing
    ).
add_cond_element(_,_). % ignore anything else; could also be an error

% Condition Variable install / deinstall
%
dynamic_install_condition_variable(Cname,V) :- condition_variable(V,Cname), !. % already there
dynamic_install_condition_variable(Cname,V) :- V = Vn:Vt, atom(Vn), atom(Vt), !,
    assert( condition_variable(V,Cname) ),
    declared_condition_variables(CondVars1),
    CondVars = [V|CondVars1],
    retractall(declared_condition_variables(_)),
    assert(declared_condition_variables(CondVars)).

dynamic_deinstall_condition_variable(Cname,V:_) :-
    retractall( condition_variable(V:_,Cname) ),
    declared_condition_variables(CVs), subtract(CVs,[V:_],CVs1),
    assert(declared_condition_variables(CVs1)).

% Condition Predicate install / deinstall
%
dynamic_install_condition_predicate(_Cname,Pname,Pargs) :- atom(Pname), is_list(Pargs),
    compound_name_arguments(Pred,Pname,Pargs),
    condition_predicate(Pred,_,_), !. % declaration already there, do nothing
dynamic_install_condition_predicate(Cname,Pname,Pargs) :- atom(Cname), atom(Pname), is_list(Pargs), !,
    compound_name_arguments(Pred,Pname,Pargs),
    retractall( condition_predicate(Pred,_,_) ), % retract any with same name, should be unnecessary
    assert( condition_predicate(Pred,Cname,undefined) ),
    declared_condition_predicates(CurrentPredicates),
    AllPredicates = [Pred|CurrentPredicates],
    retractall(declared_condition_predicates(_)),
    assert(declared_condition_predicates(AllPredicates)).

% TODO following needs Cname
dynamic_deinstall_condition_predicate(P,A) :- % TODO this needs to be condition_predicate/3
    retractall( condition_predicate(P,A) ), compound_name_arguments(Pred,P,A),
    declared_condition_predicates(CPs), subtract(CPs,[Pred],CPs1),
    retractall(declared_condition_predicates(_)),
    assert(declared_condition_predicates(CPs1)).
    % TODO must also clear the predicate definition


% delete condition elements that aren't predefined
% TODO move this above dynamic_install...

delete_cond_elements(Cname,_) :- is_cond_name(Cname,Elts), !,
    delete_cond_elements1(Cname,Elts),
    retractall( cond_name(Cname,_) ).

delete_cond_elements1(_,[]).
delete_cond_elements1(Cname,[Elt|EltList]) :-
    delete_cond_element(Cname,Elt),
    delete_cond_elements1(Cname,EltList).

% TODO following needs Cname
% delete_cond_element(_,_). % for now ignore deletes as dup adds are handled
delete_cond_element(_, condition_variable(V:_)) :- predefined_condition_variable(V:_), !. % don't delete
delete_cond_element(_, condition_variable(V:_)) :- !, dynamic_deinstall_condition_variable(_,V:_).
delete_cond_element(_, condition_predicate(P,A)) :-
    predefined_condition_predicate(P,PA), length(A,N), length(PA,N), % could also check types
    !. % don't delete
delete_cond_element(_, condition_predicate(P,A)) :- !, dynamic_deinstall_condition_predicate(P,A).
delete_cond_element(_, (H:-_)) :-
    functor(H,F,N), predefined_condition_predicate(F,FA), length(FA,N), !.  % don't delete
delete_cond_element(_, (H:-_)) :- !, functor(H,F,N), functor(HT,F,N),
    (   clause(HT,_)
    ->  retractall( HT )
    ;   true
    ).

% ------------------------------------------------------------------------
%
% CONDITION NAMES
%
:- dynamic cond_name/2.

cond_name(predefined,[]). % conditions defined in this file
cond_name(static,[]). % conditions defined in conditions.pl static init file
cond_name(dynamic,[]). % conditions defined with paapi/loadcondi but w/o a name
%cond_name(all,[]).
% ... dynamically created/deleted names associated with CElement list

is_cond_name(Cname) :- is_cond_name(Cname,_), !.
is_cond_name(Cname,Elts) :- atom(Cname), !, ( cond_name(Cname,Elts) ; Cname==all ), !.

special_cond_names([predefined,static,dynamic]).
is_special_cond_name(Cname) :- atom(Cname), special_cond_names(SN), memberchk(Cname,SN), !.

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

%  c_associate(Policy,UA,OpSet,OA) :- !, associate(Policy,UA,OpSet,OA).  %TEMPORARY FOR TESTING

c_associate(Policy,UA,OpSet,OA) :- % hook for conditional associations
    associate(Policy,UA,OpSet,OA),
    (   cond(Policy, ConditionExpr, associate(UA,OpSet,OA))
    ->  evaluate_condition(ConditionExpr)
    ;   true
    ).

c_associate(Policy,UA,OpSet,OA,CondArg) :- % hook for conditional associations with arguments
    associate(Policy,UA,OpSet,OA),
    (   cond(Policy, ConditionExpr, associate(UA,OpSet,OA))
    ->  evaluate_condition_with_args(ConditionExpr,CondArg)
    ;   true
    ).

% ------------------------------------------------------------------------
% ------------------------------------------------------------------------
% CONDITION VARIABLE DECLARATIONS
%
%   condition_variable(VariableName : VariableType)
%
%   condition_variable(zero:number).
%   condition_variable(today:name).
%
%   condition_variables([zero:number,today:name]).

:- dynamic condition_variable/1. % condition variable declaration
:- multifile condition_variable/1.

:- dynamic condition_variables/1.
:- multifile condition_variables/1.

% the actual database of condition variables:
:- dynamic condition_variable/2. % carries V:T and condition group name

% predefined condition variables below
%   e.g.: condition_variable(VariableName : VariableType).
predefined_condition_variable(zero:number).
% predefined_condition_variable(today:name). % this one mapped to a context variable
predefined_condition_variable(timestamp_now:number). % the raw timestamp value
predefined_condition_variable(datetime_now:datetime). % short date structure
predefined_condition_variable(date_now:date). % date only structure
predefined_condition_variable(time_now:time). % time oly structure

predefined_condition_variable(time_string_now:name).
predefined_condition_variable(day_now:name). % these 3 are from time of local host
predefined_condition_variable(hour_now:number).
predefined_condition_variable(minute_now:number).

predefined_condition_variable(weekday:boolean).
% ...


init_predefined_condition_variables :- % assumes clean init or re-init, call only once
    findall( CV, predefined_condition_variable(CV), CVs ),
    install_condition_variables(predefined,CVs),
    declared_condition_variables(Declared),
    append(Declared,CVs,Declared1),
    retractall(declared_condition_variables(_)),
    assert(declared_condition_variables(Declared1)).

%:- dynamic condition_variables/1.
%:- multifile condition_variables/1.

% condition_variables([zero:number,local_day:name,local_hour:number,local_minute:number]).
% % pre-defined

:- dynamic declared_condition_variables/1.

declared_condition_variables([]).

is_cond_var_def_list([]).
is_cond_var_def_list([C|Cs]) :- cond_var_list_item(C), is_cond_var_def_list(Cs).

cond_var_list_item(CVar=Val) :- atom(CVar), atomic(Val), % TODO could do more with type
	validate_condition_variable(CVar).

% ------------------------------------------------------------------------


init_static_condition_variables :-
    findall(CV, condition_variable(CV), CVs),
    install_condition_variables(static,CVs),
    declared_condition_variables(Declared), append(Declared,CVs,Declared1),
    retractall(declared_condition_variables(_)),
    assert(declared_condition_variables(Declared1)).

install_condition_variables(_,[]).
install_condition_variables(Cname,[Variable|Variables]) :-
    install_condition_variable(Cname,Variable),
    install_condition_variables(Cname,Variables).

install_condition_variable(Cname,V) :-
    assert( condition_variable(V,Cname) ).

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
    condition_variable(CondVar:Type,_), !,
    (   condition_context_variable_map(CondVar,CtxVar)
    ->  read_context_cache(CtxVar:Type,Val)
    ;   read_local_condition_variable(CondVar,Val)
    ).
condition_variable_value(_,undefined).

% ------------------------------------------------------------------------

read_local_condition_variable(Name,Val) :- atom(Name), var(Val), !,
    local_condition_variable_value(Name,Val).

% define local condition variable values
% below at % ...
%
local_condition_variable_value(zero,0) :- !.

local_condition_variable_value(timestamp_now,Stamp) :- !, get_time(Stamp).

local_condition_variable_value(datetime_now,DateTime) :- !, % this may be used for comparisons
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,Hour,Min,Sec,_,_,_),
    DateTime = datetime(Year,Month,Date,Hour,Min,Sec).

local_condition_variable_value(date_now,DateOnly) :- !, % this may be used for comparisons
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,_Hour,_Min,_Sec,_,_,_),
    DateOnly = date(Year,Month,Date).

local_condition_variable_value(time_now,TimeOnly) :- !, % this may be used for comparisons
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(_Year,_Month,_Date,Hour,Min,Sec,_,_,_),
    TimeOnly = time(Hour,Min,Sec).

%
local_condition_variable_value(time_string_now,Time) :- !, % this may not be useful for comparisons
    get_time(Stamp),
    %stamp_date_time(Stamp,LongDate,local),
    %LongDate = date(Year,Month,Date,Hour,Min,Sec,_,_,_),
    %format(atom(Time), '~d-~d-~d ~d:~d:~d',[Year,Month,Date,Hour,Min,Sec]).
    %format_time(atom(Time), '%F %T %Z', Stamp, posix).
    timestamp2text(Stamp,Time).

local_condition_variable_value(day_now,DayOfWeek) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,_Hour,_Min,_Sec,_,_,_),
    day_of_the_week(date(Year,Month,Date),Day),
    nth1(Day,['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'],DayOfWeek).

local_condition_variable_value(hour_now,Hour) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(_Year,_Month,_Date,Hour,_Min,_Sec,_,_,_).

local_condition_variable_value(minute_now,Min) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(_Year,_Month,_Date,_Hour,Min,_Sec,_,_,_).

local_condition_variable_value(weekday,B) :- !,
    local_condition_variable_value(day_now,Day),
    (   memberchk(Day,['Monday','Tuesday','Wednesday','Thursday','Friday'])
    ->  B = true
    ;   B = false
    ).

% ...

local_condition_variable_value(_,undefined).

% ------------------------------------------------------------------------
% ------------------------------------------------------------------------
% CONDITION PREDICATE DECLARATIONS

:- dynamic condition_predicate/2. % condition predicate declaration
:- multifile condition_predicate/2.

% the actual database of condition predicates:
:- dynamic condition_predicate/3. % condition_predicate(PredSig,CondGroup,Defined)

:- dynamic declared_condition_predicates/1.
:- dynamic requested_condition_predicates/1.

declared_condition_predicates([]).

requested_condition_predicates([]).

% for predefined condition predicates, place
% declarations and definitions below e.g.:
% condition_predicate(PredicateName, ListOfPredArgTypes).
%
% declarations of predefined condition predicates
%
%   datetime is a date/6 structure
%   date is a date/3 structure
%   time is a time/3 structure
%   timetext3339 is a date and time in rfc3339 (iso_8601 compatible) format
%   timetext is equivalent to timetext3339
%
%   e.g.predefined_condition_predicate(cp1, [number,boolean]).
predefined_condition_predicate(unix, []).
predefined_condition_predicate(windows, []).
predefined_condition_predicate(is_True, [boolean]).
predefined_condition_predicate(is_False, [boolean]).
predefined_condition_predicate(datetime_in_range, [datetime,datetime,datetime]).
predefined_condition_predicate(date_in_range, [date,date,date]).
predefined_condition_predicate(time_in_range, [time,time,time]).
predefined_condition_predicate(timestamp_in_range, [timestamp,timestamp,timestamp]).
predefined_condition_predicate(timetext_in_range, [timetext,timetext,timetext]).
predefined_condition_predicate(timetextrange_in_range, [timetext,timetext,timetext,timetext]).
predefined_condition_predicate(channel_in_channels, [any,list]).
predefined_condition_predicate(gbox_in_gbox, [number,number,number,number,number,number,number,number]).
% ...


init_predefined_condition_predicates :- % assumes clean init or re-init, call only once
    findall(Predicate,
            ( predefined_condition_predicate(N,A), compound_name_arguments(Predicate,N,A) ),
            Predicates),
    install_condition_predicates(predefined,Predicates),
    declared_condition_predicates(Declared),
    append(Declared,Predicates,Declared1),
    retractall(declared_condition_predicates(_)),
    assert(declared_condition_predicates(Declared1)).

% ------------------------------------------------------------------------
% This is called by dpl to request conditions needed for a policy.
% Can expand on this later to do availability checks or deferred
% availability.

request_conditions(Conditions) :-
    check_requested_conditions(Conditions),
    assert(requested_condition_predicates(Conditions)).

check_requested_conditions(_). % detail later, check syntax, etc.

% ------------------------------------------------------------------------
%

init_static_condition_predicates :- % assumes condition_predicate already exists for predefined predicates
    findall(Predicate,
            (   condition_predicate(PredName, PredArgs), compound_name_arguments(Predicate,PredName,PredArgs) ),
            Predicates),
    install_condition_predicates(static,Predicates),
    declared_condition_predicates(CurrentPredicates),
    append(CurrentPredicates,Predicates,AllPredicates),
    retractall(declared_condition_predicates(_)),
    assert(declared_condition_predicates(AllPredicates)).

install_condition_predicates(_,[]).
install_condition_predicates(Cname,[Predicate|Predicates]) :-
    install_condition_predicate(Cname,Predicate),
    install_condition_predicates(Cname,Predicates).

install_condition_predicate(Cname,P) :-
    compound_name_arguments(P,Name,Args), length(Args,N), functor(T,Name,N),
    (   clause(T,_)
    ->  D = defined
    ;   D = undefined
    ),
    assert( condition_predicate(P,Cname,D) ).


% ------------------------------------------------------------------------
% CONDITION PREDICATE DEFINITIONS (predefined)
%
unix :- current_prolog_flag(unix,true).
windows :- current_prolog_flag(windows,true).
is_True(X) :- atom(X), (X==true;X=='True'), !.
is_False(X) :- atom(X), (X==false;X=='False'), !.

% Timestamp formats
%   Cross-CPP date-time format e.g. "2020-01-01T02:02:02.000Z"
%
% Time relations
%
datetime_in_range( datetime(EYear,EMonth,EDate,EHour,EMin,ESec),
                   datetime(TYear,TMonth,TDate,THour,TMin,TSec),
                   datetime(LYear,LMonth,LDate,LHour,LMin,LSec) ) :- !,
    date_time_stamp( date(EYear,EMonth,EDate,EHour,EMin,ESec,0,-,-), ETS ),
    date_time_stamp( date(TYear,TMonth,TDate,THour,TMin,TSec,0,-,-), TTS ),
    date_time_stamp( date(LYear,LMonth,LDate,LHour,LMin,LSec,0,-,-), LTS ),
    timestamp_in_range(TTS,ETS,LTS).

date_in_range( date(EYear,EMonth,EDate),
               date(TYear,TMonth,TDate),
               date(LYear,LMonth,LDate) ) :- !,
    date_time_stamp( date(EYear,EMonth,EDate,0,0,0,0,-,-), ETS ),
    date_time_stamp( date(TYear,TMonth,TDate,0,0,0,0,-,-), TTS ),
    date_time_stamp( date(LYear,LMonth,LDate,0,0,0,0,-,-), LTS ),
    timestamp_in_range(TTS,ETS,LTS).

time_in_range( time(EHour,EMin,ESec),
               time(THour,TMin,TSec),
               time(LHour,LMin,LSec) ) :- !,
    date_time_stamp( date(1970,1,1,EHour,EMin,ESec,0,-,-), ETS ),
    date_time_stamp( date(1970,1,1,THour,TMin,TSec,0,-,-), TTS ),
    date_time_stamp( date(1970,1,1,LHour,LMin,LSec,0,-,-), LTS ),
    timestamp_in_range(TTS,ETS,LTS).

timestamp_in_range(TestTS,EarlyTS,LateTS) :- !,
    EarlyTS =< TestTS, TestTS =< LateTS.

% following used by Cross-CPP

timetext_in_range(TT,TTlow,TThigh) :- !, % not testing range
    ui:text2timestamp(TT,TS), ui:text2timestamp(TTlow,TSlow), ui:text2timestamp(TThigh,TShigh),
    timestamp_in_range(TS,TSlow,TShigh).

timetextrange_in_range(TTrl,TTrh,TTlow,TThigh) :- !, % not testing ranges
    ui:text2timestamp(TTrl,TSrl), ui:text2timestamp(TTrh,TSrh),
    ui:text2timestamp(TTlow,TSlow), ui:text2timestamp(TThigh,TShigh),
    % timestamp_in_range calculation duplicated inline for efficiency
    % timestamp_in_range(TSrl,TSlow,TShigh), timestamp_in_range(TSrh,TSlow,TShigh).
    TSlow =< TSrl, TSrl =< TShigh,  TSlow =< TSrh, TSrh =< TShigh.

channel_in_channels(C,Cs) :- atomify([C],[A]), atomify(Cs,Csa), memberchk(A,Csa). %atomify before chk

    atomify([],[]) :- !.
    atomify([N|Ns],[A|As]) :- number(N), !,
        atom_number(A,N), atomify(Ns,As).
    atomify([N|Ns],[N|As]) :- atom(N), !, atomify(Ns,As).

gbox_in_gbox(B1LongMin,B1LongMax,B1LatMin,B1LatMax,B2LongMin,B2LongMax,B2LatMin,B2LatMax) :-
    B2LongMin =< B1LongMin, B1LongMin =< B2LongMax, B2LongMin =< B1LongMax, B1LongMax =< B2LongMax,
    B2LatMin =< B1LatMin, B1LatMin =< B2LatMax, B2LatMin =< B1LatMax, B1LatMax =< B2LatMax.

%...
value_in_range(V,L,H) :- !, L =< V, V =< H.

% for internal testing only:
cond_test :- % time(H,M,S)
    time_in_range( time(7,25,00), time(7,30,00), time(7,45,00) ),
    \+ time_in_range( time(7,35,00), time(7,30,00), time(7,45,00) ),
    date_in_range( date(2020,06,01), date_now, date(2020,06,30) ),
    true.


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
%     <scalar constant> ::= <boolean constant> | <number> | <name> | <time>
%
%     <boolean constant> ::= true | 'True' | false | 'False'
%
% following time-related are new proposed additions:
%     <time> ::= <time constant> | <time placeholder>
%
%     <time constant> ::= date(<year>,<month>,<day>)
%         | date(<year>,<month>,<day>,<hour>,<minute>,<second>,<t-offset>,<TZ>,<DST>)
%         | time(<hour>,<minute>,<second>)
%         | timestamp(<seconds since Epoch>)
%
%     <time placeholder> ::= yesterday | today | tomorrow
%
%     <list constant> ::= [ <constant list items> ]
%
%     <constant> ::= <scalar constant>
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

% If a query with condition is made several preconditions are checked
% if any of the preconditions below fail, evaluate the rule condition
%    not the same condition predicate name or number of arguments
evaluate_condition_with_args(C,true) :- evaluate_condition(C).
evaluate_condition_with_args(A,CVvals) :- atom(A), is_list(CVvals), !,
    substitute_arg(CVvals,A,Aval), % A is a single variable or a constant
    evaluate_condition(Aval).
evaluate_condition_with_args(R,CVvals) :- is_list(CVvals), !,
    % CVvals is a list of condition variable definitions
    R =.. [Rf|Ra],
    maplist(substitute_arg(CVvals),Ra,Fa),
    F =.. [Rf|Fa],
	 % if Fa is to be checked for any ‘undefined’ values
    % could do it here with memberchk(undefined,Fa) and fail
    % or do it in a new clause of evaluate_condition/1
    evaluate_condition(F). % This may lead to extra unnecessary substitution pass
evaluate_condition_with_args(R,Q) :- % check if rule and query conditions are compatible
    functor(R,C,N), functor(Q,C,N), % same functor and number of arguments
    R =.. RL, Q =..QL, length(RL,N1), length(QL,N1),
    RL = [_|Ra], QL = [_|Qa],
    % check that the rule condition has variable arguments
    % instead of checking could just process the condition args from the rule and the query
    % There are two approaches:
    %   Strict:
    %      The rule condition and the query condition may use the anon var _ in any position
    %      the rule condition args are unified pairwise with the query condition args
    %         to get the final arg list. A unification failure of any arg pair or if the final
    %         argument list still has a variable as arg (as in both the rule and the query used _)
    %            then the condition will be summarily evaluated to false and the rule will not be used
    %   Permissive:
    %      Only arguments appearing in the rule condition as _ will be substituted with the
    %      corresponding argument from the query condition and other argument values in the
    %      query condition will be ignored. As in Strict, if there are residual occurrences
    %      of _ in the final argument list the condition will be summarily evaluated to false and the rule will not be used
    %
    % for evaluation construct condition with new final arg list
    maplist(subst_cargs,Ra,Qa,Fa), F =.. [C|Fa],
    evaluate_condition(F).
evaluate_condition_with_args(C,_) :- evaluate_condition(C).

subst_cargs(Ra, Qa, Qa) :- var(Ra), !. % substitute query arg if rule arg is a variable
subst_cargs(Ra, _, Ra) :- !. % otherwise use rule arg even if args conflict

evaluate_condition(C) :- ground(C), !, % Don’t evaluate if the condition has variables in it
	 C =.. L, \+memberchk(undefined,L), % Don’t evaluate the condition if anything is ‘undefined’
    evaluate_gcondition(C).

evaluate_gcondition(C) :- is_True(C), !. % constant true
evaluate_gcondition(C) :- is_False(C), !, fail. % constant false
evaluate_gcondition(C) :- validate_condition_variable(C,boolean), !,
    condition_variable_value(C,V), V == true.
evaluate_gcondition(C) :-
    validate_built_in_binary_relation(C), !,
    substitute_args(C,S,2),
    evaluate_relation(S).
evaluate_gcondition(C) :-
    validate_condition_predicate(C,N), !,
    substitute_args(C,CS,N),
    evaluate_condition_predicate(CS).

evaluate_condition_predicate(P) :-
    % may need something more here (a complete meta-interpreter)
    % e.g. should probably check argument types,
    %   predicates may contain references to condition variables
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
    substitute_cargs(Args1,Args2),
    true.

substitute_cargs(A,B) :- substitute_cargs(A,B,[]).

substitute_cargs([],[],_).
substitute_cargs([A1|Args1],[A2|Args2],CVdefs) :-
    substitute_arg(CVdefs,A1,A2),
    substitute_cargs(Args1,Args2,CVdefs).

substitute_arg(CVdefs,A1,A2) :- condition_variable(A1:T1, _), !,
    % get value from CVdefs or from condition variable store
    % value of variable from CVdefs takes priority over stored value
    (   memberchk(A1=A2, CVdefs),
        check_pred_arg(A2,T1) % is A2 is compatible with T1
    ->  true
    ;   condition_variable_value(A1,A2)
    ).
substitute_arg(_,A,A) :- !. % validate_constant(A), !.
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
                    is_greater_than_or_equal_to(number,number)
                   ]).


validate_constant(X) :- boolean_constant(X), !.
validate_constant(X) :- atom(X), !.
validate_constant(X) :- is_list(X), ground(X), !.

boolean_constant(X) :- (is_True(X) ; is_False(X)), !.

validate_condition_variable(V) :- validate_condition_variable(V,_), !.
validate_condition_variable(V,T) :- atom(V),
    declared_condition_variables(CVs), memberchk(V:T,CVs).
    % could just do condition_variable(V:T,_).

validate_condition_predicate(P,_Nargs) :- atom(P), !,
    declared_condition_predicates(CPs),
    compound_name_arity(GP,P,0),
    memberchk(GP,CPs).
validate_condition_predicate(P,Nargs) :-
    compound_name_arguments(P,PN,Pargs), % P =.. [PN|Pargs],
    length(Pargs,Nargs),
    compound_name_arity(GP,PN,Nargs),
    compound_name_arguments(GP,PN,Gargs), % GP =.. [PN|Gargs],
    % maybe could just do: condition_predicate(PN,Gargs)
    declared_condition_predicates(CPs),
    memberchk(GP,CPs),
    check_pred_args(Pargs,Gargs).

check_pred_args([],[]).
check_pred_args([PA|PAs],[GA|GAs]) :-
    check_pred_arg(PA,GA),
    check_pred_args(PAs,GAs).

% check that the first argument is compatible with the type given by the
% second argument
check_pred_arg(P,var) :- var(P), !. % allow variable for conditional queries
check_pred_arg(P,Type) :- atom(P), validate_condition_variable(P,Type), !.
check_pred_arg(P,list) :- is_list(P), !.
check_pred_arg(P,boolean) :- (P==true ; P==false ; P=='True' ; P=='False'), !.
check_pred_arg(P,number) :- number(P), !.
check_pred_arg(P,timetext) :- atom(P), parse_time(P,_), !. % could actually parse it
check_pred_arg(P,timestamp) :- number(P), !.
check_pred_arg(P,name) :- atom(P), !.
check_pred_arg(time(_,_,_),time) :- !.
check_pred_arg(date(_,_,_),date) :- !.
check_pred_arg(date(_,_,_,_,_,_),date) :- !.
check_pred_arg(datetime(_,_,_,_,_,_),datetime) :- !.
check_pred_arg(P,any) :- ground(P), !.

