
# Aim:
# Down load the nessary xlsx file 
source("02scripts/packages.R")

# source the soil water measurements --------------------------------------


url = "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/SVS_PotatoOnion_SoilWater.xlsx"

sheet = "SoilWaterMainData"
GET(url, authenticate(Sys.getenv("USERNAME"), Sys.getenv("PASSWORD"),
                      type = "ntlm"), 
    write_disk(tf <- tempfile(fileext = ".xlsx"), overwrite = TRUE)) 
list.files(tf)
df = read_excel(tf, sheet, skip = 9,.name_repair = "universal") %>% 
  as.data.table()


# source the cliflo PET ---------------------------------------------------
devtools::install_github("ropensci/clifro", ref = "iss05")
options(RCurlOptions = list(proxy = 'http://proxy.pfr.co.nz:8080'),
                          proxyusername  = Sys.getenv("USERNAME"), 
                          proxypassword  = Sys.getenv("PASSWORD"),
                          ssl.verifypeer = FALSE)
cf_user()
## Credentials 
me = cf_user(Sys.getenv("clifro_usr"),
             Sys.getenv("clifro_pass"))

## Datatypes 

my.dts = cf_datatype(select_1 =     c(9), 
                     select_2 =     c(1), 
                     check_box = list(4), 
                     combo_box =    c(NA))
my.dts

## Station 
agentno <- 17603L
my.station <- cf_station(agentno)

## Retrieve data
cf.datalist <- cf_query(user = me,
                        datatype = my.dts,
                        station = my.station,
                        start_date = "2020-07-01 00",
                        end_date = Sys.Date())
