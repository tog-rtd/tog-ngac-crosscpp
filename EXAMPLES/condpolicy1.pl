policy('CondPolicy1','Conditional Access', [
       conditions([current_day_is_one_of(list)]),

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
      /* cond( current_day_is_one_of(['Monday','Tuesday','Wednesday','Thursday','Friday']),
             associate('GroupB',[r,w],'GrB-Secret') ),*/
       cond( is_member_of(local_day,['Monday','Tuesday',/*'Wednesday',*/'Thursday','Friday']),
             associate('GroupB',[r,w],'GrB-Secret') ),
       associate('Division',[r],'Projects')
]).

