library(targets)
source(here::here("02scripts/simpleSWD.R"))
targets::tar_option_set(packages = c("data.table","httr","readxl","magrittr",
                                     "DBI","RPostgreSQL","clifro","here"))

list(
  tar_target(iplant_url, 
             "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/SVS_PotatoOnion_SoilWater.xlsx"
             ),
  tar_target(tempfile, 
             download_excel(iplant_url)
             ),
  tar_target(dt_soilwater,
             read_excel(tempfile, sheet = "SoilWaterMainData", skip = 9,.name_repair = "universal") %>% 
               as.data.table()
             ),
  tar_target(dt_irrigation, 
             read_excel(tempfile, sheet = "IrrigationDiary", skip = 4,.name_repair = "universal") %>% 
               as.data.table()
             ),
  tar_target(dt_error,
             read_excel(tempfile, sheet = "SWdata_metadata", skip = 9, .name_repair = "universal") %>% 
               as.data.table()
             ),
# clean up the date -------------------------------------------------------
  tar_target(df,
             change_tz(dt_soilwater)
             ),
  tar_target(df_irrigation, 
             change_tz(dt_irrigation)
             ),
  tar_target(df_error,
             change_tz(dt_error)
             ),

# retrieve climate data ---------------------------------------------------
  tar_target(me,
             cf_user(Sys.getenv("clifro_usr"),
                    Sys.getenv("clifro_pass"))
             ),
  tar_target(my.dts,
             cf_datatype(select_1 =     c(9, 3),
                         select_2 =     c(1, 1),
                         check_box = list(4, 1),
                         combo_box =    c(NA, NA))
             ),
  tar_target(my.station,
             cf_station(17603L)
             ),
  tar_target(start_date, 
             df$Date[1]
             ),
  tar_target(cf.datalist, 
             cf_query(user = me,
                      datatype = my.dts,
                      station = my.station,
                      start_date = paste(start_date, "00"),
                      end_date = Sys.Date())
             ),

# clean up the cliflo data ------------------------------------------------
  tar_target(PET, 
             as.data.table(cf.datalist[[1]])[, Date := as.Date(`Date(local)`, 
                                                               format = "%Y-%m-%d")
                                             ][, .(Date, PET = `Amount(mm)`)]
             ),
  tar_target(Rain, 
             as.data.table(cf.datalist[[2]])[, Date := as.Date(`Date(local)`, 
                                                               format = "%Y-%m-%d", tz = "NZ")
                                             ][, .(Date, Rain = `Amount(mm)`)]
             )
)