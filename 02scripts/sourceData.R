
# Aim:
# Down load the nessary xlsx file 
source("02scripts/packages.R")
source("02scripts/simpleSWD.R")
# source the soil water measurements --------------------------------------

# options(RCurlOptions = list(proxy = 'http://proxy.pfr.co.nz:8080'),
#         proxyusername  = Sys.getenv("USERNAME"), 
#         proxypassword  = Sys.getenv("PASSWORD"),
#         ssl.verifypeer = FALSE)
url = "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/SVS_PotatoOnion_SoilWater.xlsx"

sheet = "SoilWaterMainData"
tf <- download_excel(url) 
file.exists(tf)
df <-  read_excel(tf, sheet, skip = 9,.name_repair = "universal") %>% 
  as.data.table()
df[, Date := as.Date(Date, tz = "NZ")]
excel_sheets(tf)
df_irrigation <-  read_excel(tf, sheet = "IrrigationDiary", skip = 4,.name_repair = "universal") %>% 
  as.data.table()
df_error <- read_excel(tf, sheet = "SWdata_metadata", skip = 9, .name_repair = "universal") %>% 
  as.data.table()
df_irrigation[, Date := as.Date(Date, tz = "NZ")]
df_error[, Date := as.Date(Date, tz = "NZ")]

## Tidy the soil moisture data 

df[!is.na(Crop)]

# source the cliflo PET ---------------------------------------------------

## Manual download

PET <- fread(here::here("01raw-data/PET_RAIN.genform1_proc"), skip = 8)
Rain <- fread(here::here("01raw-data/PET_RAIN.genform1_proc"), skip = "Rain: Daily")
## Fix the date
# Somehow NIWA's datetime step can not be transfer directly to match excel datetime
PET <- PET[, Date := as.Date(`Date(NZST)`, format = "%Y,%m,%d", tz = "NZ")
           ][, .(Date, PET = `Amount(mm)`)]
Rain <- Rain[, Date := as.Date(`Date(NZST)`, format = "%Y,%m,%d", tz = "NZ")
             ][, .(Date, Rain = `Amount(mm)`)]

# devtools::install_github("ropensci/clifro", ref = "iss05")

cf_user()
## Credentials 
# me = cf_user(Sys.getenv("clifro_usr"),
#              Sys.getenv("clifro_pass"))
# 
# ## Datatypes 
# 
# my.dts = cf_datatype(select_1 =     c(9), 
#                      select_2 =     c(1), 
#                      check_box = list(4), 
#                      combo_box =    c(NA))
# my.dts
# 
# ## Station 
# agentno <- 17603L
# my.station <- cf_station(agentno)
# 
# ## Retrieve data
# cf.datalist <- cf_query(user = me,
#                         datatype = my.dts,
#                         station = my.station,
#                         start_date = "2020-07-01 00",
#                         end_date = Sys.Date())
