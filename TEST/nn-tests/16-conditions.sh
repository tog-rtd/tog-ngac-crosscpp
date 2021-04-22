#!/bin/sh
# this script tests manipulation of the conditions database
#
curl -s -G "http://127.0.0.1:8001/paapi/loadcondi" --data-urlencode "cond_elements=[
    condition_variable(my_devid:name), condition_variable(my_mchan:number),
    condition_variable(my_tstart:timetext), condition_variable(my_tstop:timetext),
    condition_variable(my_tsubmit:timetext), condition_variable(my_loMin:number),
    condition_variable(my_loMax:number), condition_variable(my_laMin:number), condition_variable(my_laMax:number),
    condition_predicate( my_dr_offer_5f5a39f2b559dcf200f424d0, [name,number,timetext,timetext,timetext,number,number,number,number] ),
    (  my_dr_offer_5f5a39f2b559dcf200f424d0(Dev,Chan,Start,Stop,Submit,LoMin,LoMax,LaMin,LaMax) :-
        Dev == '95b40cf9-a9fc-4bd8-b695-99773b6f25e4', channel_in_channels( Chan, [1, 2] ),
        timetextrange_in_range( Start, Stop, '2019-09-10T14:36:34.682Z', '2020-09-10T14:36:34.682Z' ),
        timetext_in_range( Submit, '2019-09-10T14:36:34.682Z', '2020-09-10T14:36:34.682Z' ),
        gbox_in_gbox( LoMin, LoMax, LaMin, LaMax, -9.39, 4.3, 35.95, 43.75) )
    ]" --data-urlencode "token=admin_token" --data-urlencode "cond_name=my_cond1"
#
#

