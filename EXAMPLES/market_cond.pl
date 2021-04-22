condition_variable(devid:name).
condition_variable(mchan:number).
condition_variable(tstart:timetext).
condition_variable(tstop:timetext).
condition_variable(tsubmit:timetext).
condition_variable(loMin:number).
condition_variable(loMax:number).
condition_variable(laMin:number).
condition_variable(laMax:number).
condition_predicate( dr_offer_5f5a39f2b559dcf200f424d0, [name,number,timetext,timetext,timetext,number,number,number,number] ).
(dr_offer_5f5a39f2b559dcf200f424d0(Dev,Chan,Start,Stop,Submit,LoMin,LoMax,LaMin,LaMax) :-
    Dev == '95b40cf9-a9fc-4bd8-b695-99773b6f25e4', channel_in_channels( Chan, [1, 2] ),
    timetextrange_in_range( Start, Stop, '2019-09-10T14:36:34.682Z', '2020-09-10T14:36:34.682Z' ),
    timetext_in_range( Submit, '2019-09-10T14:36:34.682Z', '2020-09-10T14:36:34.682Z' ),
    gbox_in_gbox( LoMin, LoMax, LaMin, LaMax, -9.39, 4.3, 35.95, 43.75) ).
