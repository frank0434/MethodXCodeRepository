
# Aim:
# Down load the nessary xlsx file 
source("C:/Data/SVS/02scripts/packages.R")
source("C:/Data/SVS/02scripts/simpleSWD.R")
# source the soil water measurements --------------------------------------

## Iplant address that has the soil water measurements 
url <- "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/SVS_PotatoOnion_SoilWater.xlsx"
## Sheet name for the raw soil water content 
sheet <- "SoilWaterMainData"
## Download the excel file to a temp directory 
tf <- download_excel(url) 
## Check if the file exisits
file.exists(tf)
## Read data in 
df <-  read_excel(tf, sheet, skip = 9,.name_repair = "universal") %>% 
  as.data.table()
## The irrigation data 
df_irrigation <-  read_excel(tf, sheet = "IrrigationDiary", skip = 4,.name_repair = "universal") %>% 
  as.data.table()
## Manual correction about the neutron probe data 
df_error <- read_excel(tf, sheet = "SWdata_metadata", skip = 9, .name_repair = "universal") %>% 
  as.data.table()
# Read Diary for guessing the plant growth stage ---------------------------
url_diary <- "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/DIARY%20for%20SVS%20Potato-Onion%20rotation.xlsx"
sheet_diary <- "Diary"
tf_diary <- download_excel(url) 
## Check if the file exisits
file.exists(tf_diary)
## Read data in 
df_diary <-  read_excel(tf_diary, sheet_diary, skip = 18, .name_repair = "universal") %>% 
  as.data.table()


## Convert the date to NZ timezone 
df[, Date := as.Date(Date, tz = "NZ")]
df_irrigation[, Date := as.Date(Date, tz = "NZ")]
df_error[, Date := as.Date(Date, tz = "NZ")]

## Tidy the soil moisture data 


# source the cliflo PET ---------------------------------------------------

## Manual download

# PET <- fread(here::here("01raw-data/PET_RAIN.genform1_proc"), skip = 8)
# Rain <- fread(here::here("01raw-data/PET_RAIN.genform1_proc"), skip = "Rain: Daily")
## Fix the date
# Somehow NIWA's datetime step can not be transfer directly to match excel datetime
# PET <- PET[, Date := as.Date(`Date(NZST)`, format = "%Y,%m,%d", tz = "NZ")
#            ][, .(Date, PET = `Amount(mm)`)]
# Rain <- Rain[, Date := as.Date(`Date(NZST)`, format = "%Y,%m,%d")
#              ][, .(Date, Rain = `Amount(mm)`)]



# auto --------------------------------------------------------------------


# devtools::install_github("ropensci/clifro", ref = "iss05")

# cf_user()
## Credentials 
me = cf_user("linc3423",
             "broadfield")
# 
# ## Datatypes 
# PET and daily rainfall
my.dts = cf_datatype(select_1 =     c(9, 3),
                     select_2 =     c(1, 1),
                     check_box = list(4, 1),
                     combo_box =    c(NA, NA))
# my.dts
# 
# ## Station 
agentno <- 17603L
my.station <- cf_station(agentno)
# The starting date
startd <- df$Date[1]
# ## Retrieve data
cf.datalist <- cf_query(user = me,
                        datatype = my.dts,
                        station = my.station,
                        start_date = paste(startd, "00"),
                        end_date = Sys.Date())

PET <- as.data.table(cf.datalist[[1]])[, Date := as.Date(`Date(local)`, 
                                                         format = "%Y-%m-%d")
                                       ][, .(Date, PET = `Amount(mm)`)]

Rain <- as.data.table(cf.datalist[[2]])[, Date := as.Date(`Date(local)`, 
                                                         format = "%Y-%m-%d", tz = "NZ")
                                        ][, .(Date, Rain = `Amount(mm)`)]
