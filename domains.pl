% Domains
:- module(domains, [gaccess_check/3]).


% Local cloud policy decision points PDPi are augmented by a global
% policy decision point PDPg.
%
% Local cloud PDPs for Web services run on the local cloud's gateway.
% The global PDPg may run on each local cloud gateway or on a separate
% device.
%
% The gateways share a Global Communications Policy with which all
% instances of the PDPg are initialized.
% The GCP contains a Gateway-to-Gateway (GG) Policy that determines
% whether a particular gateway-to-gateway communication is permitted.
% The GG is made up of g_association rules, which are considered
% directional from the first named gateway to the second. E.g.:
%
%     g_associate( g1, [invoke], g3 ).
%
% The GCP may also contain a Cloud-to-Cloud (CC) Policy made up of
% c_association rules. These rules associate "external attributes" (EAs)
% in two clouds with an operation set, similar to associations between
% user attributes and object attributes in a local cloud policy.
%
%     c_associate( ea(lc1:g1:ea11), Opset, ea(lc2:g2:ea21) ).
%
% For clouds M and N, a reference by a service consumer in cloud M to a
% service provider in cloud N is subject to a composed policy
%     ( policyN, GG, CC, policyM )
%
% There is a "proxy pair" of agents that provide the appearance that the
% service consumer directly accesses the service provider while
% providing mediation to enforce the composed policy.
% The gateway in the service consumer's local cloud acts as the first
% half of the proxy pair and the gateway in the service provider's local
% cloud acts as the second half.


%:- use_module(library(http/thread_httpd)).
%:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/http_wrapper)).
%:- use_module(library(http/http_header)).
%:- use_module(library(http/http_parameters)).

:- use_module(audit,[audit_gen/2]).
:- use_module(param).
:- use_module(dpl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gaccess_check(Policy, (Source, Operation, Destination))
%   Lc - Local Cloud
%   s, S - source
%   d, D - destination
%   Op - operation

gaccess_check(LcS:S,Op,LcD:D) :-
	atom(LcS), atom(S), atom(Op), atom(LcD), atom(D), !,

	param:current_gpolicy(GGPolicy),
	(   GGPolicy == none
	->  fail
	;   true
	),
	param:current_cpolicy(CCPolicy),
	(   CCPolicy == none
	->  fail
	;   true
	),

	%local_cloud_policy(LcS,LcSpolicy),
	%local_cloud_policy(LcD,LcDpolicy),

	LcSpolicy = LcS, LcDpolicy = LcD,
	policy(LcSpolicy, LcSPC),
	policy(LcDpolicy, LcDPC),
	cc_local_cloud_gateway(CCPolicy,LcS,Gs),
	cc_local_cloud_gateway(CCPolicy,LcD,Gd),

	gg_associate(GGPolicy, Gs, GGops, Gd),
	memberchk(Op,GGops),

	associate(LcSpolicy:LcSPC, UAs, LcSops, ATs),
	cc_external_attribute(CPolicy,ATs),
	memberchk(Op,LcSops),

	associate(LcDpolicy:LcDPC, UAd, LcDops, ATd),
	cc_external_attribute(CPolicy,UAd),
	memberchk(Op,LcDops),

	cc_associate(CPolicy, ATs, CCops, UAd),
	memberchk(Op,CCops),

	pdp:is_contained_in_ua(LcSpolicy:LcSPC, S, UAs),
	pdp:is_contained_in_oa(LcDpolicy:LcDPC, D, ATd),
	!.


