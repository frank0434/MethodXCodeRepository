library(targets)
source(here::here("02scripts/functions.R"))
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
             ),

# calculation  ------------------------------------------------------------
  tar_target(cols,
             grep("^\\w", colnames(df), value = TRUE)
             ),
  tar_target(DT, 
             df[!is.na(Crop) & !is.na(Date), cols, with = FALSE]
             ),
  tar_target(colnames, # Constant
             colnames(DT)
             ),
  tar_target(value_var,
             grep("VWC.+", colnames, value = TRUE)
             ),
  # Cautious!!!!! SW is in percentage
  tar_target(thickness, # constant
             2),
  tar_target(id_var,
             c("Crop", "Date", "Field_Plot_No", "Plot_No","Irrigation...8", 
               "N_rate")),
  # Missing or erroneous values will be removed by join the metadata which has 
  # the information about bad values
  tar_target(metadata,
             melt.data.table(df_error, measure = patterns("VWC_*"))
             ),
  ## Remove comment column and filter down which value was missing 
  tar_target(NAcells,
             metadata[, Comments:=NULL][value == 1]),
  ## Get the raw measurements into long format 
  tar_target(DT_long, 
             melt(DT, id.vars = id_var, measure.vars = value_var, 
                  variable.factor = FALSE)[, value := as.numeric(value)]),
  ## merge the raw with manual correction table
  tar_target(DTwithmeta_withNA, 
             merge.data.table(DT_long, NAcells, 
                              by.x = c("Date", "Field_Plot_No", "variable"),
                              by.y = c("Date", "Plot", "variable"), 
                              all.x = TRUE, suffixes = c("", ".y"))
             ),
  ## remove the bad ones
  tar_target(DTwithmeta,  
             DTwithmeta_withNA[is.na(value.y)
                               ][, value.y := NULL]),
  # value is doubled to get the mm unit, `thickness` holds the converter. 
  tar_target(DT_summariesed, 
             DTwithmeta[, .(SW = mean(as.numeric(value)*thickness, na.rm = TRUE)), 
                        by = .(Crop, Date, variable, Irrigation...8, N_rate)]
             ), 
  # Transfer layer information to integer layers
  tar_target(layers_name,
             unique(DT_summariesed$variable)), 
  ## Hard code layer 
  tar_target(layers_no, c(1, 6, 7, 8, 2, 3, 4,5))


)