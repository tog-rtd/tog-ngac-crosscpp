er_package(er_test2, [
    er(
        ev_pat(user(any),policy_class(any),operation(adnull),object(any)),
        [addm([]),deletem([])]
    ),
    er(
        ev_pat(user(any),policy_class(any),operation(addone),object(any)),
        [add('Policy (a)',user(u4))]
    ),
    er(
        ev_pat(user(any),policy_class(any),operation(deletem1),object(any)),
        [
         deletem('Policy (a)',[
                     user(u4),
                     assign(u2,'Group2'),
                     user(u2)
              ])
        ]),
    er(
        ev_pat(user(any),policy_class(any),operation(addm1),object(any)),
        [
         addm('Policy (a)',[
                  user(u2),
                  assign(u2,'Group2')
                 ])
        ]),
    er(
        ev_pat(user(any),policy_class(any),operation(deletem2),object(any)),
        [
         deletem('Policy (a)',[
                     assign(u2,'Group2'),
                     user(u2)
              ])
        ]),
    er(
        ev_pat(user(any),policy_class(any),operation(addm2),object(any)),
        [
         addm('Policy (a)',[
                  user(u2),
                  assign(u2,'Group2')
                 ])
        ])
]).
