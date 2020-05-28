:- module(pmcmd, [cmd/3,
		  pmCmdStr_CmdToks/2, pmCmdStr_CmdTerm/2, pmCmdStrs_CmdTerms/2,
		  pmCmdTerm_CmdToks/2
		 ]).
:- style_check(-singleton).





% cmd( CmdTerm, CmdToks, CmdArgs )
cmd( add(a(UattrName), c('PM')), [add, a, UattrName, c, 'PM'], undef ) :- !. %, [UattrName] ).
cmd( add(a(UattrName), p(PolName)), [add, a, UattrName, p, PolName], undef ) :- !. %, [UattrName, PolName] ).
cmd( add(a(UattrName), a(Uattr2Name)), [add, a, UattrName, a, Uattr2Name], undef ) :- !. %, [UattrName, Uattr2Name] ).
cmd( add(a(UattrName), u(UserName)), [add, a, UattrName, u, UserName], undef ) :- !. %, [UattrName, UserName] ).

cmd( add(a(UattrName), as(AttrSetName)), [add, a, UattrName, as, AttrSetName], undef ) :- !. %, [UattrName, AttrSetName] ).
cmd( add(u(UserName), c('PM')), [add, u, UserName, c, 'PM'], undef ) :- !. %, [UserName] ).
cmd( add(u(UserName), a(UattrName)), [add, u, UserName, a, UattrName], undef ) :- !. %, [UserName, UattrName] ).

cmd( add(p(PolName), c('PM')), [add, p, PolName, c, 'PM'], cmdAddPc ) :- !. %, [PolName] ).

cmd( add(s(OpsetName), oc(ObjClass), a(UattrName)), [add, s, OpsetName, oc, ObjClass, a, UattrName], undef ) :- !. %, [OpsetName, ObjClass, UattrName] ).
cmd( add(s(OpsetName), oc(ObjClass), b(OattrName)), [add, s, OpsetName, oc, ObjClass, b, OattrName], undef ) :- !. %, [OpsetName, ObjClass, OattrName] ).
cmd( add(s(OpsetName), oc(ObjClass), c('PM')), [add, s, OpsetName, oc, ObjClass, c, 'PM'], undef ) :- !. %, [OpsetName, ObjClass] ).
cmd( add(op(Operation), s(OpsetName)), [add, op, Operation, s, OpsetName], undef ) :- !. %, [Operation, OpsetName] ).
cmd( add(op(Operation), oc(ObjClass)), [add, op, Operation, oc, ObjClass], undef ) :- !. %, [Operation, ObjClass] ).
cmd( add(op(Operation), deny(DenyName)), [add, op, Operation, deny, DenyName], undef ) :- !. %, [Operation, DenyName] ).
cmd( add(oc(ObjClass)), [add, oc, ObjClass], undef ) :- !. %, [ObjClass] ).
cmd( add(b(OattrName), c('PM')), [add, b, OattrName, c, 'PM'], undef ) :- !. %, [OattrName] ).
cmd( add(b(OattrName), p(PolName)), [add, b, OattrName, p, PolName], undef ) :- !. %, [OattrName, PolName] ).
cmd( add(b(OattrName), b(Oattr2Name)), [add, b, OattrName, b, Oattr2Name], undef ) :- !. %, [OattrName, Oattr2Name] ).
cmd( add(b(OattrName), deny(DenyName)), [add, b, OattrName, deny, DenyName], undef ) :- !. %, [OattrName, DenyName] ).
cmd( add(cb(OattrName), deny(DenyName)), [add, cb, OattrName, deny, DenyName], undef ) :- !. %, [OattrName, DenyName] ).
cmd( add(ob(ObjName, ObjClass, Inh, HostOrOrigName, PathOrIgnored, BaseNodeType, BaseNodeName)), [add, ob, ObjName, ObjClass, Inh, HostOrOrigName, PathOrIgnored, BaseNodeType, BaseNodeName], undef ) :- !. %, [ObjName, ObjClass, Inh, HostOrOrigName, PathOrIgnored, BaseNodeType, BaseNodeName] ).

% cmd( add(prop(Name=Value), EntityType, EntityName), [add, prop, Name=Value, EntityType, EntityName], [Name=Value, EntityType, EntityName] ).
cmd( add(prop(Name=Value), p(PolName)), [add, prop, Name=Value, p, PolName], undef ) :- !. %, [Name=Value, PolName] ).
cmd( add(prop(Name=Value), a(UattrName)), [add, prop, Name=Value, a, UattrName], undef ) :- !. %, [Name=Value, UattrName] ).
cmd( add(prop(Name=Value), b(OattrName)), [add, prop, Name=Value, b, OattrName], undef ) :- !. %, [Name=Value, OattrName] ).

cmd( add(as(AttrSetName)), [add, as, AttrSetName], undef ) :- !. %, [AttrSetName] ).

cmd( add(a(UattrName), as(AttrSetName)), [add, a, UattrName, as, AttrSetName], undef ) :- !. %, [UattrName, AttrSetName] ).

cmd( add(sa(SacName)), [add, sa, SacName], undef ) :- !. %, [SacName] ).

cmd( add(as(AttrSetName), sa(SacName)), [add, as, AttrSetName, sa, SacName], undef ) :- !. %, [AttrSetName, SacName] ).

cmd( add(app(AtPath, EditorPath, WkfPath), h(Host)), [add, app, AtPath, EditorPath, WkfPath, h, Host], undef ) :- !. %, [AtPath, EditorPath, WkfPath, Host] ).

cmd( add(ks(UserKeyStore, UserTrustStore), h(Host), u(User)), [add, ks, UserKeyStore, UserTrustStore, h, Host, u, User], undef ) :- !. %, [UserKeyStore, UserTrustStore, Host, User] ).

cmd( add(deny(DenyName), DenyType, u(UserName), IsIntersection), [add, deny, DenyName, DenyType, u, UserName, IsIntersection], undef ) :- !. %, [DenyName, DenyType, UserName, IsIntersection] ).
cmd( add(deny(DenyName), DenyType, a(UattrName), IsIntersection), [add, deny, DenyName, DenyType, a, UattrName, IsIntersection], undef ) :- !. %, [DenyName, DenyType, UattrName, IsIntersection] ).

cmd( add(op(Operation), deny(DenyName)), [add, op, Operation, deny, DenyName], undef ) :- !. %, [Operation, DenyName] ).
cmd( add(b(OattrName), deny(DenyName)), [add, b, OattrName, deny, DenyName], undef ) :- !. %, [OattrName, DenyName] ).
cmd( add(cb(OattrName), deny(DenyName)), [add, cb, OattrName, deny, DenyName], undef ) :- !. %, [OattrName, DenyName] ).

% cmd( asg(ent1type(Ent1Name), ent2type(Ent2Name)) ). %, [Ent1Name, Ent2Name] ).
cmd( asg(a(Name1), a(Name2)), [asg, a, Name1, a, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(a(Name1), b(Name2)), [asg, a, Name1, b, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(a(Name1), s(Name2)), [asg, a, Name1, s, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(a(Name1), p(Name2)), [asg, a, Name1, p, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(b(Name1), a(Name2)), [asg, b, Name1, a, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(b(Name1), b(Name2)), [asg, b, Name1, b, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(b(Name1), s(Name2)), [asg, b, Name1, s, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(b(Name1), p(Name2)), [asg, b, Name1, p, Name2], undef ) :- !. %. %], [Name1, Name2] ).
cmd( asg(s(Name1), a(Name2)), [asg, s, Name1, a, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(s(Name1), b(Name2)), [asg, s, Name1, b, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(s(Name1), s(Name2)), [asg, s, Name1, s, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(s(Name1), p(Name2)), [asg, s, Name1, p, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(u(Name1), a(Name2)), [asg, u, Name1, a, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(u(Name1), b(Name2)), [asg, u, Name1, b, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(u(Name1), s(Name2)), [asg, u, Name1, s, Name2], undef ) :- !. %, [Name1, Name2] ).
cmd( asg(u(Name1), p(Name2)), [asg, u, Name1, p, Name2], undef ) :- !. %, [Name1, Name2] ).


%
% PM Command format conversions
%
%   conversions among the three forms: the external command string, the
%   tokenized command string, and the Prolog term. The cmd/2 predicate
%   provides the conversion from term to token list.
%
%   CmdStr - external command string of the PM engine as an atom
%   CmdToks - list of tokens of a PM external command (without '|' separators)
%   CmdTerm - term form of a command used in Prolog code
%
%   The simple syntax of the CmdStr makes it "token-izable" with
%   atomic_list_concat/3 using '|' as the token delimiter. Conversion to
%   a command string cannot be done with a token list that is not fully
%   instantiated (ground). A CmdStr is always ground. CmdToks or
%   CmdTerm may exist in non-ground states, e.g., in cmd/2.

pmCmdStr_CmdToks(CmdStr, CmdToks) :- % -CmdStr, +CmdToks OR +CmdStr, -CmdToks
	( (var(CmdStr), ground(CmdToks)) ; (ground(CmdStr), var(CmdToks)) ), !,
        atomic_list_concat(CmdToks,'|',CmdStr).

% corresponding Command String and Command Term
pmCmdStr_CmdTerm(CmdStr, CmdTerm) :- % +CmdStr, -CmdTerm
	ground(CmdStr), var(CmdTerm), !,
	(   atom_prefix(CmdStr,'#') % convert comment
	->  sub_atom(CmdStr,1,_,0,CommentBody),
	    atom_concat('%',CommentBody,CmdTerm)
	;   pmCmdStr_CmdToks(CmdStr,CmdToks),
	    cmd(CmdTerm, CmdToks, _)
	).
pmCmdStr_CmdTerm(CmdStr, CmdTerm) :- % -CmdStr, +CmdTerm
	var(CmdStr), ground(CmdTerm), !,
	(   ( atom(CmdTerm), atom_prefix(CmdTerm,'%')) % convert comment
	->  sub_atom(CmdTerm,1,_,0,CommentBody),
	    atom_concat('#',CommentBody,CmdStr)
	;   cmd(CmdTerm,CmdToks,_),
	    pmCmdStr_CmdToks(CmdStr,CmdToks)
	).

% lists of corresponding Command Strings and Command Terms
pmCmdStrs_CmdTerms(CmdStrs, CmdTerms) :- % +CmdStrs, -CmdTerms OR -CmdStrs, +CmdTerms
	maplist(pmCmdStr_CmdTerm, CmdStrs, CmdTerms).


pmCmdTerm_CmdToks(CmdTerm, CmdToks) :- % ?CmdTerm, ?CmdToks
	(nonvar(CmdTerm) ; nonvar(CmdToks)), !,
	cmd(CmdTerm, CmdToks, _).

