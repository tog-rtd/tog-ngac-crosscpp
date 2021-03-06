policy('Policy4','Privileged-Access', [

       user(u1),
       user(u2),
       user(u3),

       user_attribute(ordinary_user),
       user_attribute(administrative_user),
       user_attribute(all_users),

       object(o1),
       object(o2),
       object(o3),
       object(o4),

       object_attribute(unrestricted_object),
       object_attribute(restricted_object),
       object_attribute(all_objects),
 
       policy_class('Privileged-Access'),

       connector('PM'),

       assign(u1,ordinary_user),
       assign(u2,ordinary_user),
       assign(u3,administrative_user),

       assign(ordinary_user,all_users),
       assign(administrative_user,all_users),

       assign(o1,unrestricted_object),
       assign(o2,unrestricted_object),
       assign(o3,restricted_object),
       assign(o4,restricted_object),

       assign(unrestricted_object,all_objects),
       assign(restricted_object,all_objects),

       assign(all_users,'Privileged-Access'),
       assign(all_objects,'Privileged-Access'),

       assign('Privileged-Access','PM'),

       associate(ordinary_user,[read],unrestricted_object),
       associate(administrative_user,[read,write],all_objects)
]).

