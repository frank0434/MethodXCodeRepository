library(targets)
source("02scripts/simpleSWD.R")
targets::tar_option_set(packages = c("data.table","httr","readxl","magrittr",
                                     "DBI","RPostgreSQL","clifro"))

list(
  tar_target(iplant_url, 
             "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/SVS_PotatoOnion_SoilWater.xlsx"
             ),
  tar_target(tempfile, 
             download_excel(iplant_url)
             ),
  tar_target(df,
             read_excel(tempfile, sheet = "SoilWaterMainData", skip = 9,.name_repair = "universal") %>% 
               as.data.table()
             ),
  tar_target(df_irrigation, 
             read_excel(tempfile, sheet = "IrrigationDiary", skip = 4,.name_repair = "universal") %>% 
               as.data.table())
)