policy('SimpleRoles','Simple Roles', [
		user(u1),
		user(u2),
		user(u3),
		
		user_attribute(simple_user),
		user_attribute(admin_user),
		user_attribute(all_users),
		
		object(dk),
		object(di),
		object(dsd),
		object(dsm),
		
		object_attribute(unrestricted),
		object_attribute(restricted),
		object_attribute(resources),
		
		policy_class('Simple Roles'),
		
		connector('PM'),
		
		assign(u1,simple_user),
		assign(u2,simple_user),
		assign(u3,admin_user),
		
		assign(simple_user,all_users),
		assign(admin_user,all_users),
		
		assign(dk,unrestricted),
		assign(di,unrestricted),
		assign(dsd,unrestricted),
		assign(dsm,restricted),
		
		assign(unrestricted,resources),
		assign(restricted,resources),
		
		assign(all_users,'Simple Roles'),
		assign(resources,'Simple Roles'),
		
		assign('Simple Roles','PM'),
		
		associate(simple_user,[g],unrestricted),
		associate(admin_user,[c,r,u,g],resources)
]).
