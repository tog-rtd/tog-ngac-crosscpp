% ------------------------------------------------------------------------
% CONDITION VARIABLE DECLARATIONS
%   condition_variable(VariableName : VariableType)
%   Type is one of: list, boolean, number, name

condition_variable(weekday:boolean).
condition_variable(business:boolean).

%condition_variable(condVar1:number).
%condition_variable(condVar2:name).
%condition_variable(condVar3:boolean).
%condition_variable(lockdown:boolean).
%condition_variable(today:name).

% ------------------------------------------------------------------------
% CONDITION PREDICATE DECLARATIONS & DEFINITIONS
%   condition_predicate(PredicateName,PredicateArgs)
%   PredicateArgs is a list of Types
%   Each Type is one of: list, boolean, number, name, any
%

condition_predicate(is_weekday, []).
condition_predicate(is_business, []).

condition_predicate(current_day_is_one_of, [list]).
%condition_predicate(not_lockdown, []).

% condition predicate definitions

is_weekday :-
    condition_variable_value(weekday,W), W == true.

is_business :-
    condition_variable_value(business,B), B == true.

current_day_is_one_of(SetOfDays) :-
    condition_variable_value(local_day,Today),
    memberchk(Today,SetOfDays).

%not_lockdown :-
%    condition_variable_value(lockdown,L), L == false.
