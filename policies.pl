:- module(policies, [policy/3,gg_policy/2,cc_policy/2]).
% Example policies used for built-in self-test
%   All policies defined in this module will be automatically included
%   in the initialization of the policy database in dpl/pip.
%   'Policy (a)', 'Policy (b)', 'Signals Access Policy' and 'Vehicle Ownership Policy'
%   and others that may be added up to the comment END OF TEST POLICIES
%   are assumed to be available to the self tests for pdp/pip.
%   DO NOT REMOVE THEM!
%   It is OK to add others.
%
% policy(PolicyName, PolicyRoot, PolicyElements)
% gg_policy(PolicyName, PolicyRoot, PolicyElements)
% cc_policy(PolicyName, PolicyRoot, PolicyElements)

:- dynamic policy/3, gg_policy/2, cc_policy/2.
:- discontiguous policy/3, gg_policy/2, cc_policy/2.

%
% Do not modify the following 5 policies

policy('Policy (a)','Project Access', [
	user('u1'),
	user('u2'),
	user_attribute('Group1'),
        user_attribute('Group2'),
        user_attribute('Division'),
	object('o1'),
        object('o2'),
        object('o3'),
        object_attribute('Project1'),
        object_attribute('Project2'),
        object_attribute('Gr2-Secret'),
        object_attribute('Projects'),
	policy_class('Project Access'),
	connector('PM'),
	assign('u1','Group1'),
	assign('u2','Group2'),
	assign('Group1','Division'),
	assign('Group2','Division'),
	assign('o1','Project1'),
	assign('o2','Project2'),
	assign('o3','Gr2-Secret'),
	assign('Project1','Projects'),
	assign('Project2','Projects'),
	assign('Division','Project Access'),
	assign('Projects','Project Access'),
	assign('Gr2-Secret','Project Access'),
        assign('Project Access','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Group1',[w],'Project1'),
	associate('Group2',[w],'Project2'),
	associate('Group2',[r,w],'Gr2-Secret'),
	associate('Division',[r],'Projects')
	]).

policy('Policy (b)','File Management', [
	user('u1'),
	user('u2'),
	user_attribute('Alice'),
	user_attribute('Bob'),
	user_attribute('Users'),
	object('o2'),
	object('o3'),
	object('o4'),
	object_attribute('Proposals'),
	object_attribute('Reports'),
	object_attribute('Bob Home'),
	policy_class('File Management'),
	connector('PM'),
	assign('u1','Alice'),
	assign('u2','Bob'),
	assign('Alice','Users'),
	assign('Bob','Users'),
	assign('o2','Proposals'),
	assign('o3','Reports'),
	assign('o4','Reports'),
	assign('Proposals','Bob Home'),
	assign('Reports','Bob Home'),
	assign('Users','File Management'),
	assign('Bob Home','File Management'),
        assign('File Management','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Bob',[r,w],'Bob Home'),
	associate('Alice',[r,w],'o2')
	]).

policy('Signals Access Policy','Signals Access', [
        user('Sebastian'),
        user('Ana'),
        user('OEM employee 1'),

        user_attribute('Vehicle Owners'),
        user_attribute('Vehicle OEM'),

        object('VIN-1001 Shift Signals'),
        object('VIN-1001 Window Signals'),
        object('VIN-1001 Door Signals'),
        object('VIN-1001 Trip Signals'),

        object('VIN-1002 Shift Signals'),
        object('VIN-1002 Window Signals'),
        object('VIN-1002 Door Signals'),
        object('VIN-1002 Trip Signals'),

        object('VIN-2001 Shift Signals'),
        object('VIN-2001 Window Signals'),
        object('VIN-2001 Door Signals'),
        object('VIN-2001 Trip Signals'),

        object('VIN-3001 Shift Signals'),
        object('VIN-3001 Window Signals'),
        object('VIN-3001 Door Signals'),
        object('VIN-3001 Trip Signals'),

        object_attribute('Trip Signals'),
        object_attribute('Window Signals'),
        object_attribute('Door Signals'),
        object_attribute('Shift Signals'),

        object_attribute('Owner Accessible Signals'),
        object_attribute('OEM Accessible Signals'),

        policy_class('Signals Access'),

        connector('PM'),

        assign('Sebastian', 'Vehicle Owners'),
        assign('Ana', 'Vehicle Owners'),

        assign('OEM employee 1', 'Vehicle OEM'),

        assign('Vehicle OEM', 'Vehicle Owners'),

        assign('VIN-1001 Shift Signals', 'Shift Signals'),
        assign('VIN-1001 Window Signals', 'Window Signals'),
        assign('VIN-1001 Door Signals', 'Door Signals'),
        assign('VIN-1001 Trip Signals', 'Trip Signals'),

        assign('VIN-1002 Shift Signals', 'Shift Signals'),
        assign('VIN-1002 Window Signals', 'Window Signals'),
        assign('VIN-1002 Door Signals', 'Door Signals'),
        assign('VIN-1002 Trip Signals', 'Trip Signals'),

        assign('VIN-2001 Shift Signals', 'Shift Signals'),
        assign('VIN-2001 Window Signals', 'Window Signals'),
        assign('VIN-2001 Door Signals', 'Door Signals'),
        assign('VIN-2001 Trip Signals', 'Trip Signals'),

        assign('VIN-3001 Shift Signals', 'Shift Signals'),
        assign('VIN-3001 Window Signals', 'Window Signals'),
        assign('VIN-3001 Door Signals', 'Door Signals'),
        assign('VIN-3001 Trip Signals', 'Trip Signals'),

        assign('Trip Signals', 'OEM Accessible Signals'),

        assign('Window Signals', 'Owner Accessible Signals'),
        assign('Door Signals', 'Owner Accessible Signals'),
        assign('Shift Signals', 'Owner Accessible Signals'),

        assign('Owner Accessible Signals', 'OEM Accessible Signals'),

        assign('Vehicle Owners', 'Signals Access'),
        assign('OEM Accessible Signals', 'Signals Access'),
        assign('Owner Accessible Signals', 'Signals Access'),

        assign('Signals Access','PM'),

        associate('Vehicle Owners', [r], 'Owner Accessible Signals'),
        associate('Vehicle OEM', [r,w], 'OEM Accessible Signals')
]).

policy('Vehicle Ownership Policy','Vehicle Ownership', [
	user('Sebastian'),
        user('Ana'),

	user_attribute('Scholze Family'),
        user_attribute('Correia Family'),

        user_attribute('Owners'),

	object('VIN-1001 Shift Signals'),
	object('VIN-1001 Window Signals'),
	object('VIN-1001 Door Signals'),
	object('VIN-1001 Trip Signals'),

	object('VIN-1002 Shift Signals'),
	object('VIN-1002 Window Signals'),
	object('VIN-1002 Door Signals'),
	object('VIN-1002 Trip Signals'),

	object('VIN-2001 Shift Signals'),
	object('VIN-2001 Window Signals'),
	object('VIN-2001 Door Signals'),
	object('VIN-2001 Trip Signals'),

	object('VIN-3001 Shift Signals'),
	object('VIN-3001 Window Signals'),
	object('VIN-3001 Door Signals'),
	object('VIN-3001 Trip Signals'),

	object_attribute('Vehicle VIN-1001'),
	object_attribute('Vehicle VIN-1002'),
	object_attribute('Vehicle VIN-2001'),
	object_attribute('Vehicle VIN-3001'),

	object_attribute('Scholze Family Vehicles'),
	object_attribute('Correia Family Vehicles'),

	object_attribute('Vehicles'),

	policy_class('Vehicle Ownership'),

	connector('PM'),

	assign('Sebastian', 'Scholze Family'),
	assign('Ana', 'Correia Family'),

	assign('Scholze Family', 'Owners'),
	assign('Correia Family', 'Owners'),

	assign('VIN-1001 Shift Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Window Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Door Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Trip Signals', 'Vehicle VIN-1001'),

	assign('VIN-1002 Shift Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Window Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Door Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Trip Signals', 'Vehicle VIN-1002'),

	assign('VIN-2001 Shift Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Window Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Door Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Trip Signals', 'Vehicle VIN-2001'),

	assign('VIN-3001 Shift Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Window Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Door Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Trip Signals', 'Vehicle VIN-3001'),

	assign('Vehicle VIN-1001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-1002', 'Correia Family Vehicles'),
	assign('Vehicle VIN-2001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-3001', 'Correia Family Vehicles'),

	assign('Scholze Family Vehicles', 'Vehicles'),
	assign('Correia Family Vehicles', 'Vehicles'),

	assign('Owners', 'Vehicle Ownership'),
	assign('Vehicles', 'Vehicle Ownership'),

	assign('Vehicle Ownership', 'PM'),

	associate('Scholze Family',[o,r],'Scholze Family Vehicles'),
	associate('Correia Family',[o,r],'Correia Family Vehicles')
	]).

policy('CondPolicy1','Conditional Access', [
        conditions([is_weekday, current_day_is_one_of(list)]),

        user('u1'),
        user('u2'),

        user_attribute('GroupA'),
        user_attribute('GroupB'),
        user_attribute('Division'),

        object('o1'),
        object('o2'),
        object('o3'),

        object_attribute('ProjectA'),
        object_attribute('ProjectB'),
        object_attribute('GrB-Secret'),
        object_attribute('Projects'),

        policy_class('Conditional Access'),
        connector('PM'),

        assign('u1','GroupA'),
        assign('u2','GroupB'),
        assign('GroupA','Division'),
        assign('GroupB','Division'),
        assign('o1','ProjectA'),
        assign('o2','ProjectB'),
        assign('o3','GrB-Secret'),
        assign('ProjectA','Projects'),
        assign('ProjectB','Projects'),
        assign('Division','Conditional Access'),
        assign('Projects','Conditional Access'),
        assign('GrB-Secret','Conditional Access'),
        assign('Conditional Access','PM'),

        associate('GroupA',[w],'ProjectA'),
        associate('GroupB',[w],'ProjectB'),
        %cond( current_day_is_one_of(['Monday','Tuesday',/*'Wednesday',*/'Thursday','Friday']),
        %cond( is_member_of(local_day,['Monday','Tuesday','Wednesday','Thursday','Friday']),
        cond( is_weekday,
              associate('GroupB',[r,w],'GrB-Secret') ),
        associate('Division',[r],'Projects')
        ]).

% Cloud of Clouds example

policy(lc1,'Local Cloud 1',[
       user(c11), user(c12),
       object(p11), object(p12), object(p13),
       user_attribute(cgroup1), user_attribute(cgroup2),
       object_attribute(pgroup1), object_attribute(pgroup2),
       assign(c11,cgroup1),
       assign(c12,cgroup2),
       assign(p11,pgroup1),
       assign(p12,pgroup1),
       assign(p13,pgroup2),

       associate(cgroup1,[invoke],pgroup1),
       associate(cgroup2,[invoke],pgroup2),

       external_attribute(ea11), % outgoing references

       associate(cgroup1, [invoke], ea11)
       ]).

policy(lc2,'Local Cloud 2',[
       user(c21), user(c22), user(c23),
       object(p21), object(p22),
       user_attribute(group2),
       object_attribute(resources2),

       assign(p21,resources2),
       assign(c22,group2), assign(c23,group2),
       % ...

       external_attribute(ea21), % outgoing references
       external_attribute(ea22), % incoming references

       associate(group2, [invoke], ea21),
       associate(ea22, [invoke], resources2)
       ]).

policy(lc3,'Local Cloud 3',[
       user(c31), user(c32),
       object(p31), object(p32), object(p33),
       user_attribute(project3),
       object_attribute(resources3),
       % ...
       assign(p32,resources3),

       external_attribute(ea31), % outgoing references
       external_attribute(ea32), % incoming references

       associate(project3, [invoke], ea31),
       associate(ea32, [invoke], resources3)
       ]).

gg_policy('Gateway Flows', [
        gateway(g1),
        gateway(g2),
        gateway(g3),
        gg_associate(g1, [invoke], g3),
        gg_associate(g1, [invoke], g2),
        gg_associate(g2, [invoke], g3),
        gg_associate(g3, [invoke], g2)
       ]).

cc_policy('Cloud-of-Clouds', [
        % cc_associate(ExternalAttribute1, OpSet, ExternalAttribute2)
        %local_cloud_policy(lc1,lc1_policy), % name policies lc1, etc for simplicity
        %local_cloud_policy(lc2,lc2_policy),
        %local_cloud_policy(lc3,lc3_policy),
        local_cloud_gateway(lc1,g1),
        local_cloud_gateway(lc2,g2),
        local_cloud_gateway(lc3,g3),
        external_attribute(ea11),
        external_attribute(ea21),
        external_attribute(ea22),
        external_attribute(ea31),
        external_attribute(ea32),
        cc_assign(ea11,g1),
        cc_assign(ea21,g2), cc_assign(ea22,g2),
        cc_assign(ea31,g3), cc_assign(ea32,g3),
        cc_associate( ea11, [invoke], ea32 ),
        cc_associate( ea12, [invoke], ea22 ),
        cc_associate( ea21, [invoke], ea32 ),
        cc_associate( ea31, [invoke], ea22 )
       ]).

% end of Cloud of Clouds example

% END OF TEST POLICIES
% DO NOT DELETE OR MODIFY THE PRECEDING POLICIES
%
% Other example or test policies may be added below
%

policy('Policy (aa)','Project Access 1', [
	user('u1'),
	user('u2'),
        user('u3'),
        user('u5'),
	user_attribute('Group1'),
        user_attribute('Group2'),
        user_attribute('Division'),
	object('o1'),
        object('o2'),
        object('o3'),
        object_attribute('Project1'),
        object_attribute('Project2'),
        object_attribute('Gr2-Secret'),
        object_attribute('Projects'),
	policy_class('Project Access 1'),
	connector('PM'),
	assign('u1','Group1'),
	assign('u2','Group2'),
	assign('Group1','Division'),
	assign('Group2','Division'),
	assign('o1','Project1'),
	assign('o2','Project2'),
	assign('o3','Gr2-Secret'),
	assign('Project1','Projects'),
	assign('Project2','Projects'),
	assign('Division','Project Access 1'),
	assign('Projects','Project Access 1'),
	assign('Gr2-Secret','Project Access 1'),
        assign('Project Access 1','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Group1',[w],'Project1'),
	associate('Group2',[w],'Project2'),
	associate('Group2',[r,w],'Gr2-Secret'),
	associate('Division',[r],'Projects')
	]).

policy('Policy (bb)','File Management 1', [
	user('u1'),
	user('u2'),
        user('u4'),
        user('u5'),
	user_attribute('Alice'),
	user_attribute('Bob'),
	user_attribute('Users'),
	object('o2'),
	object('o3'),
	object('o4'),
	object_attribute('Proposals'),
	object_attribute('Reports'),
	object_attribute('Bob Home'),
	policy_class('File Management 1'),
	connector('PM'),
	assign('u1','Alice'),
	assign('u2','Bob'),
	assign('Alice','Users'),
	assign('Bob','Users'),
	assign('o2','Proposals'),
	assign('o3','Reports'),
	assign('o4','Reports'),
	assign('Proposals','Bob Home'),
	assign('Reports','Bob Home'),
	assign('Users','File Management 1'),
	assign('Bob Home','File Management 1'),
        assign('File Management 1','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Bob',[r,w],'Bob Home'),
	associate('Alice',[r,w],'o2')
	]).

