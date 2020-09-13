library(clifro)
me = cf_user("frank0434","havelock")

## Datatypes 

my.dts = cf_datatype(select_1 =     c(9), 
                     select_2 =     c(1), 
                     check_box = list(4), 
                     combo_box =    c(NA))
my.dts

## Station 

my.station <- cf_station(17603)
