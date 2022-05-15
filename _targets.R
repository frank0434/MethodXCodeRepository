library(targets)
source(here::here("02scripts/functions.R"))
source(here::here("02scripts/ProcessCanopyCover.R"))
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
  tar_target(value_var,
             grep("VWC.+", colnames(DT), value = TRUE)
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
  tar_target(DT_summarised_no_order,
             DTwithmeta[, .(SW = mean(as.numeric(value)*thickness, na.rm = TRUE)),
                        by = .(Crop, Date, variable, Irrigation...8, N_rate)]
             ),
  # Transfer layer information to integer layers
  tar_target(layers_name,
             unique(DT_summarised_no_order$variable)),
  ## Hard code layer
  tar_target(layers_no, c(1, 6, 7, 8, 2, 3, 4,5)),
  tar_target(DT_summarised,
             order_layer(DT_summarised = DT_summarised_no_order,
                        layers_no = layers_no, layers_name = layers_name)
             ),
  ## Pattern search was way to complex, manually pull out the critical dates
  tar_target(dates_of_interests,
             as.Date(c("2019-10-22", "2019-11-12","2020-04-29",
                       "2020-05-19", "2020-06-03", "2021-01-21",
                       "2021-03-03", "2021-06-28", 
                       "2021-09-07"))
             ),
  tar_target(event_of_interests, 
             c("Potato planted",
               "Potato emerged",
               "Potato final-harvest",
               "Wheat sown",
               "Wheat emerged",
               "Wheat final-harvest",
               "Broccoli planted",
               "Broccoli final-harvest",
               "Onion sown")),
  tar_target(growth_event,
             data.table(Date = dates_of_interests, 
                        Events = event_of_interests
                        )[, (c("Crop", "Events")) := tstrsplit(Events, split = "\\s")
                          ][Events %like% c("emerged|harvest|plant")]
             ), 
  tar_target(growth_stage_withCanopy, 
             merge.data.table(growth_event, full_canopy, all = TRUE, 
                              by = "Date")
             ),
  ## Tidy up the stages 
  tar_target(growth_stage,
             growth_stage_withCanopy[, ':='(Crop =ifelse(is.na(Crop), as.character(Var2), Crop),
                                            Events = ifelse(is.na(Events), "canopy-closure", Events),
                                            mean = ifelse(!is.na(mean), mean, 
                                                          fcase(Events %like% c("planted|harvest"), 0,
                                                                Events %like% c("emerged"),  0.1)))]
             ),
  ## Interpolate the NDVI
  ### figure out the date range
  tar_target(date_range, 
             range(growth_stage$Date)
             ),
  ### Sequence the period for interpolation 
  tar_target(full_period, 
             seq.Date(from = date_range[1], to = date_range[2], by = "day") %>% 
               data.table(Date = .)
             ),

  ### Join the full date with the growth stage
  tar_target(DT_canopy,
             merge.data.table(full_period, growth_stage[,.(Date, Events, mean, Crop)],
                              by = "Date", all = TRUE)
             ),
  ### Update columns that have NAs 
  tar_target(DT_canopy_correction,
             canopy_cover(DT_canopy)
             ),
  
  # Simple SWD --------------------------------------------------------------
  tar_target(DT_profile_simple_60cm,
             wb_simple(DT_summarised)
             ),
  tar_target(update_simpleSWD.irr1,
             dcast.data.table(DT_profile_simple_60cm[Irrigation == 1],
                              Crop + Date  + profile~ N_rate, value.var = "SWD")
             ),
  tar_target(update_simpleSWD.irr2,
             dcast.data.table(DT_profile_simple_60cm[Irrigation == 2],
                              Crop + Date  + profile~ N_rate, value.var = "SWD")
             ),
  tar_target(DT_profile_simple,
             SWD_depth(DT_summarised)
             ),
  tar_target(profile_simpleSWD.irr1,
             dcast.data.table(DT_profile_simple[Irrigation == 1],
                              Crop + Date ~ N_rate, value.var = "SWD")
             ),
  tar_target(profile_simpleSWD.irr2,
             dcast.data.table(DT_profile_simple[Irrigation == 2],
                              Crop + Date ~ N_rate, value.var = "SWD")
             ),
  # WATER BALANCE -----------------------------------------------------------
  # irrigation  -------------------------------------------------------------
  tar_target(irrigation,
             melt.data.table(df_irrigation, 
                             id.vars = c("Crop", "Date"), value.name = "Irrigation",
                             variable.factor = FALSE, variable.name = "Treatment" )
             ),
  # # 1.8 m profile ---------------------------------------------------------
  tar_target(update_irr1,
             dcast.data.table(DT_profile_simple[Irrigation == 1],
                              Crop + Date ~ N_rate,
                              value.var = "Profile")
             ),
  tar_target(update_irr2, 
             dcast.data.table(DT_profile_simple[Irrigation == 2],
                              Crop + Date ~ N_rate,
                              value.var = "Profile")
             ),
  # climate -----------------------------------------------------------------
  tar_target(PET_Rain, 
             merge(PET, Rain, by = 'Date', all.x  = TRUE)
             ),
  # # Join the irrigation  --------------------------------------------------
  tar_target(joined_input,
             merge.data.table(PET_Rain, irrigation, 
                              by = c("Date"), all.x = TRUE)
             ),
  # Transfer to wide to show in grafana
  tar_target(update_input,
             dcast.data.table(joined_input, 
                              Date + PET + Rain + Crop ~ Treatment,
                              value.var = "Irrigation")[, ':='(`NA` = NULL,
                                                               Date = as.Date(Date, tz = "NZ"))]
             ),
  # water balance -----------------------------------------------------------
  tar_target(WaterBalance_correction,
             copy(update_input)[, ':='(Irrigation.1 = ifelse(is.na(Irrigation.1), 0, Irrigation.1),
                                       Irrigation.2 = ifelse(is.na(Irrigation.2), 0, Irrigation.2))
                                ][, ':='(Precipitation.1 = Rain + Irrigation.1,
                                         Precipitation.2 = Rain + Irrigation.2)
                                  ][1, Crop := "Potato"
                                    ][, Crop := zoo::na.locf(Crop)]
             ),
  ## Join the growth stage information 
  tar_target(WaterBalance,
             wb_correction(WaterBalance_correction,DT_canopy_correction)
             ),
  tar_target(wb, 
             wb_daily(WaterBalance, DT_profile_simple, DT_summarised, 
                      maxdepth = 1)
             ),

  # Resetting values  -------------------------------------------------------
  ## The actual measurements for reset the water balance model    
  tar_target(reset_df,
             SWD_depth(DT_summarised, 
                       maxdepth = 3)[, list(realSWD = list(.SD)), 
                                     by = .(Irrigation, N_rate)]
             ),
  # PET correction with reset for top 60cm--------------------------------------
  tar_target(wb_60cm_reset_PETcor,
             wb_daily(WaterBalance, DT_profile_simple, DT_summarised, 
                      maxdepth = 3, reset = reset_df)
             ),
  tar_target(update_WaterBalance.1_60cm_reset_PETcor,
             join_wb(wb_60cm_reset_PETcor, WaterBalance, update_simpleSWD.irr1)
             ),
  tar_target(update_WaterBalance.2_60cm_reset_PETcor,
             join_wb(wb_60cm_reset_PETcor, WaterBalance, update_simpleSWD.irr2)
             )
)