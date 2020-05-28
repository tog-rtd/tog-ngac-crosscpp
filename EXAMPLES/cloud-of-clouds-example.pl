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

