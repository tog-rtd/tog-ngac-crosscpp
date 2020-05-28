policy('Policy3','rbac', [
       user('jones'),
       user('smith'),

       user_attribute('Doctor'),
       user_attribute('Intern'),

       %object('mrec1'), % this will generate a placeholder for the host and path
       object('mrec1','File',no,musial,'/PM/Examples/MedRecords/mrec1.rtf',object_attribute,'Medical Records'),

       object_attribute('Medical Records'),

       policy_class('rbac'),

       operation(read, 'File'),
       operation(write, 'File'),

       connector('PM'),

       assign('jones','Intern'),
       assign('smith','Doctor'),
       assign('Doctor','Intern'),
       assign('mrec1','Medical Records'),
       assign('Intern','rbac'),
       assign('Medical Records','rbac'),
       assign('rbac','PM'),

       associate('Doctor',[write],'Medical Records'),
       associate('Intern',[read],'Medical Records')
]).

