context_variables([ % variables obtained from the context system
    weekday:boolean,
    business:boolean %,

    %day_of_the_week:name,
    %contextVar1:number,
    %contextVar2:name,
    %contextVar3:boolean,
    %adminLockdown:boolean
]).

% these two condition variables are given the same names
% as the context variables
condition_context_variable_map(business, business).
condition_context_variable_map(weekday, weekday).

%condition_context_variable_map(today, day_of_the_week).
%condition_context_variable_map(condVar1, contextVar1).
%condition_context_variable_map(condVar2, contextVar2).
%condition_context_variable_map(condVar3, contextVar3).
%condition_context_variable_map(lockdown, adminLockdown).
