% Next Generation Access Control audit manager
:- module(audit, [audit_gen/2, audit_set/1, audit_select/1, audit_deselect/1]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).

% AUDIT EVENTS
%
%   auditable_events is the set of all auditable ngac events
%   audit_selection is the subset of auditable_events currently selected
%     to generate audit records. It can be changed with setparam, but it
%     is the responsibility of the caller of setparam to ensure that
%     only elements of auditable_events are placed in audit_selection.
%

% audit event groups are a shorthand for a group of related audit events
%
% The operations audit_set, audit_select, and audit_deselect must still
% be extended to recognize audit event groups.
%
audit_event_group(policy_query,[pq_grant,pq_deny,pq_obj_info]).
audit_event_group(policy_admin,[pa_add,pa_delete,pa_combine,pa_getpol,pa_setpol,
                                pa_load,pa_unload,pa_init_session,pa_end_session]).
audit_event_group(event_processing,[ep_load,ep_unload,ep_event]).

% This defines the set of possible auditable events
auditable_events([ngac_start, ngac_shutdown, epp_start, epp_shutdown,
                  pq_grant, pq_deny, % these not currently used
                  pq_objinfo,
                  pa_add, pa_delete, pa_combine, pa_getpol, pa_setpol, pa_load, pa_loadi, pa_unload,
                  pa_init_session, pa_end_session,
                  ep_load, ep_unload, ep_activate, ep_deactivate, ep_event, ep_command,
                  policy_admin, policy_query, gpolicy_query, event_admin, event_processing,
                  ngac_error  % do not remove general ngac_error
                 ]).
% audit_selection is the set of currently selected events to generate
% audit records. In module param: audit_selection/1 is defined as
% initially []

init(basic) :- !,
    param:setparam(audit_selection,
                   [server_start,
                    pq_grant,pq_deny,
                    pa_combine, pa_setpol, pa_load, pa_loadi,
                    ep_load, ep_unload, ep_event,
                    ngac_error
                   ]).
init(full) :- !,
    auditable_events(InitEvents), % turn all events on
    param:setparam(audit_selection,InitEvents).

% ngac audit operations
%   audit_gen generates audit record info for ngac audit
%     it leaves time stamp generation to the system audit function
%   audit_set unconditionally sets the parameter auditable_events
%     to the given list, except it will not permit the audit event
%     ngac_error to not be included.
%   audit_select addes the given list of ngac audit events to the
%     audit_selection
%   audit_deselect removes the given list of ngac audit events from the
%     current audit_selection
%
% ngac audit events
%   There is a generic event: ngac_error
%   This is used for ngac errors for otherwise unaudited ngac errors.
%   It is always an auditable event and always selected (cannot be
%   deselsected).
%

audit_gen(Event, Data) :-
    param:audit_selection(AS),
    (   memberchk(Event, AS)
    ->  sys_audit(ngac, Event, Data)
    ;   true
    ), !.
audit_gen(_,_). % always succeed

audit_set(Events) :-
    ground(Events), is_set(Events),
    auditable_events(Auditable),
    subset(Events,Auditable),
    union(Events,[ngac_error],SetEvents), % ngac_error cannot be removed from ngac audit_selection
    param:setparam(audit_selection,SetEvents), !.
audit_set(_) :-
    sys_audit(ngac, ngac_error, 'Failure to set audit_selection.').

audit_select(Event) :- atom(Event), !, audit_select([Event]).
audit_select(Events) :-
    ground(Events), is_set(Events),
    auditable_events(Auditable),
    subset(Events,Auditable),
    param:audit_selection(CurrentSelection),
    union(Events,CurrentSelection,SetEvents),
    param:setparam(audit_selection,SetEvents), !.
audit_select(_) :-
    sys_audit(ngac, ngac_error, 'Failure to add to audit_selection.').

audit_deselect(Event) :- atom(Event), !, audit_deselect([Event]).
audit_deselect(Events) :-
    ground(Events), is_set(Events),
    delete(Events,ngac_error,DeSelection), % cannot deselect ngac_error
    auditable_events(Auditable),
    subset(DeSelection,Auditable),
    param:audit_selection(CurrentSelection),
    subtract(CurrentSelection,DeSelection,NewSelection),
    param:setparam(audit_selection,NewSelection), !.
audit_deselect(_) :-
    sys_audit(ngac, ngac_error, 'Failure to delete from audit_selection.').

%
% Customise below for local system audit
%   always succeeds
%

sys_audit(Source, Event, EventData) :- atom(Source), atom(Event), ground(EventData),
    % for general use without a specific system audit service, write to local audit
    % std error is default audit_stream if audit_logging(on)
    % a local file is created for the log if audit_logging(file)
    (	\+ param:audit_logging(off)
    ->	param:audit_stream(Audit),
	gen_time_stamp(TS), % use own time stamp only for own log
	format(Audit, 'audit(~w, ~q, ~q, ~q).~n', [TS,Source,Event,EventData]),
	flush_output(Audit)
    ;	true
    ), % for general use or for testing

    %
    % Call system-specific audit
    %

    % e.g.: phantom_audit(Source,Event,EventData),

    !.
sys_audit(_,_,_).

gen_time_stamp(TS) :-
	get_time(T),
	stamp_date_time(T,date(YYYY,MM,DD,H,M,S,_,_,_),local),
	format(atom(TS), '~d-~d-~d_~d:~d:~d', [YYYY,MM,DD,H,M,truncate(S)]).

testq :- % temporary test query
    param:admin_token(Token),
    format(atom(AuthHeader),'Authorization: OAuth ~a',Token),
%    ChunkHeader='Transfer-Encoding: chunked',
    ContentHeader='Content-Type: text/plain',
%    ContentHeader='Content-Type: application/x-www-form-urlencoded',
%    http_post('http://httpbin.org:80/post','',Reply,[header(AuthHeader),header(ContentHeader)]),
    http_get('http://httpbin.org:80/get?a=aaa&b=bbb',ReplyG,[]),
    format('Reply:~n~w~n',ReplyG), !,
%    http_get('http://httpbin.org:80/get',ReplyP,[header('Authorization: OAuth eyJ0eXAiOiJKV'),header('Content-Type: text/plain')]),
%    http_post('http://httpbin.org/post','mydata',ReplyP,[header(AuthHeader),header(ContentHeader)]),
    http_post('http://httpbin.org/post?a=aaa&b=bbb','',ReplyP,[header(AuthHeader),header(ContentHeader)]),
    format('Reply:~n~w~n',ReplyP),
    true.
