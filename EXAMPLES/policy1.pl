policy('Policy1','Project Access', [

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

       policy_class('Project Access'),

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
       assign('Division','Project Access'),
       assign('Projects','Project Access'),
       assign('GrB-Secret','Project Access'),
       assign('ProjectAccess','PM'),
       associate('GroupA',[w],'ProjectA'),
       associate('GroupB',[w],'ProjectB'),
       associate('GroupB',[r,w],'GrB-Secret'),
       associate('Division',[r],'Projects')
]).

