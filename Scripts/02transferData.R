
# Aim:
# Manipulate data 

# required cols 
cols <- grep("^\\w", colnames(df), value = TRUE)

# Filtered out NAs
DT <- df[!is.na(Crop) & !is.na(Date), ..cols]

# Constant
colnames <- colnames(DT)
value_var <- grep("VWC.+", colnames, value = TRUE)
# Cautious!!!!! SW is in percentage
thickness <- 2
id_var <- c("Crop", "Date", "Field_Plot_No", "Plot_No","Irrigation...8", "N_rate")

# Missing or erroneous values will be removed by join the metadata which has the
# information about bad values
metadata <- melt.data.table(df_error, measure = patterns("VWC_*"))
## Remove comment column and filter down which value was missing 
NAcells <- metadata[, Comments:=NULL][value == 1]
## Get the raw measurements into long format 
DT_long <- melt(DT, 
                id.vars = id_var, measure.vars = value_var, 
                variable.factor = FALSE)[, value := as.numeric(value)]
## merge the raw with manual correction table
DTwithmeta <- merge.data.table(DT_long, NAcells, 
                               by.x = c("Date", "Field_Plot_No", "variable"),
                               by.y = c("Date", "Plot", "variable"), 
                               all.x = TRUE, suffixes = c("", ".y"))
## remove the bad ones
DTwithmeta <- DTwithmeta[is.na(value.y)
                         ][, value.y := NULL]
# value is doubled to get the mm unit, `thickness` holds the converter. 
DT_summariesed <- DTwithmeta[, .(SW = mean(as.numeric(value)*thickness, na.rm = TRUE)), 
                             by = .(Crop, Date, variable, Irrigation...8, N_rate)]

# Transfer layer information to integer layers
layers_name <- unique(DT_summariesed$variable)
## Hard code layer 
layers_no <- c(1,  6, 7, 8, 2, 3, 4,5)

names(layers_no) <- layers_name
DT_summariesed$variable <- layers_no[DT_summariesed$variable]

# The diary data for plant growth stages  ---------------------------------

## Bring the canopy closure data from ProcessCanopyCover.R file 


## Pattern search was way to complex, manually pull out the critical dates
dates_of_interests <- as.Date(c("2019-10-22", "2019-11-12","2020-04-29",
                                "2020-05-19", "2020-06-03", "2021-01-21",
                                "2021-03-03", "2021-06-28", 
                                "2021-09-07"))
event_of_interests <- c("Potato planted",
                        "Potato emerged",
                        "Potato final-harvest",
                        "Wheat sown",
                        "Wheat emerged",
                        "Wheat final-harvest",
                        "Broccoli planted",
                        "Broccoli final-harvest",
                        "Onion sown")
growth_event <- data.table(Date = dates_of_interests, 
                           Events = event_of_interests
                           )[, (c("Crop", "Events")) := tstrsplit(Events, split = "\\s")
                             ][Events %like% c("emerged|harvest|plant")]
growth_stage <- merge.data.table(growth_event, full_canopy, all = TRUE, by = "Date")
## Tidy up the stages 
growth_stage[, ':='(Crop =ifelse(is.na(Crop), as.character(Var2), Crop),
                    Events = ifelse(is.na(Events), "canopy-closure", Events),
                    mean = ifelse(!is.na(mean), mean, 
                                  fcase(Events %like% c("planted|harvest"), 0,
                                        Events %like% c("emerged"),  0.1)))]
## Interpolate the NDVI
### figure out the date range
date_range <- range(growth_stage$Date) 
### Sequence the period for interpolation 
full_period <- seq.Date(from = date_range[1], to = date_range[2], by = "day") %>% 
  data.table(Date = .)
### Join the full date with the growth stage
DT <- merge.data.table(full_period, growth_stage[,.(Date, Events, mean, Crop)],
                       by = "Date", all = TRUE)
### Update columns that have NAs 
DT[, ':='(Crop = zoo::na.locf(Crop), # forward fill
          Events = zoo::na.locf(Events), # forward fill
          PET_correction = fcase(mean %in% c(0, 0.1), 0.15,
                                 mean > 0.75, 1))]
### Need end value for interpolation in each group
loc_last_in_group <- DT[, .I[.N], by = .(Events, Crop)]
# loc_first_in_group <- DT[, .I[1], by = .(Events, Crop)]
### First value in each group will be the end value for the previous group
### Logic not working since the values not aligned - hard code the last values 
DT[loc_last_in_group$V1, PET_correction := c(rep(c(0.15,1,1), 3),0.15)] # Fix this 

## update the interpolation 
DT[, PET_correction:= zoo::na.approx(PET_correction, Date, na.rm = FALSE), 
   by = .(Crop, Events)]


# Simple SWD --------------------------------------------------------------
## Simple SWD uses a user-defined PAWC (usually the maximum value over a series measurement)
## SWD is calculated by subtracting the PAWC by the actual measurement

## Empty list to store things
l <- vector("list", length = 3)
Profiles <- paste(c(20, 40, 60), "cm")
for (i in seq_len(3)){
  DT_profile_simple_60cm <- SWD_depth(DT_summariesed, maxdepth = i)
  l[[i]] <- DT_profile_simple_60cm[, profile := Profiles[i]]
}
DT_profile_simple_60cm <- rbindlist(l)

update_simpleSWD.irr1 <- dcast.data.table(DT_profile_simple_60cm[Irrigation == 1],
                                          Crop + Date  + profile~ N_rate, value.var = "SWD")

update_simpleSWD.irr2 <- dcast.data.table(DT_profile_simple_60cm[Irrigation == 2],
                                          Crop + Date  + profile~ N_rate, value.var = "SWD")

DT_profile_simple <- SWD_depth(DT_summariesed)

profile_simpleSWD.irr1 <- dcast.data.table(DT_profile_simple[Irrigation == 1],
                                          Crop + Date ~ N_rate, value.var = "SWD")
profile_simpleSWD.irr2 <- dcast.data.table(DT_profile_simple[Irrigation == 2],
                                          Crop + Date ~ N_rate, value.var = "SWD")


# WATER BALANCE -----------------------------------------------------------
# irrigation  -------------------------------------------------------------
irrigation <- melt.data.table(df_irrigation, 
                              id.vars = c("Crop", "Date"), value.name = "Irrigation",
                              variable.factor = FALSE, variable.name = "Treatment" )
# # 1.8 m profile ---------------------------------------------------------
# DT_summariesed[order(Date,Plot_No, N_rate, Irrigation...8, decreasing = TRUE)]

update_irr1 <- dcast.data.table(DT_profile_simple[Irrigation == 1],
                                Crop + Date ~ N_rate,
                                value.var = "Profile")
update_irr2 <- dcast.data.table(DT_profile_simple[Irrigation == 2],
                                Crop + Date ~ N_rate,
                                value.var = "Profile")


# climate -----------------------------------------------------------------
PET_Rain <- merge(PET, Rain, by = 'Date', all.x  = TRUE)

# # Join the irrigation  --------------------------------------------------
joined_input <- merge.data.table(PET_Rain, irrigation, 
                                 by = c("Date"), 
                                 all.x = TRUE)
# Transfer to wide to show in grafana

update_input <- dcast.data.table(joined_input, 
                                 Date + PET + Rain + Crop ~ Treatment,
                                 value.var = "Irrigation")[, ':='(`NA` = NULL,
                                                                  Date = as.Date(Date, tz = "NZ"))]
# water balance -----------------------------------------------------------


WaterBalance <- copy(update_input)[, ':='(Irrigation.1 = ifelse(is.na(Irrigation.1), 0, Irrigation.1),
                                          Irrigation.2 = ifelse(is.na(Irrigation.2), 0, Irrigation.2))
                                   ][, ':='(Precipitation.1 = Rain + Irrigation.1,
                                            Precipitation.2 = Rain + Irrigation.2)
                                     ][1, Crop := "Potato"
                                       ][, Crop := zoo::na.locf(Crop)]
## Join the growth stage information 
WaterBalance <- merge.data.table(WaterBalance, DT[,.(Date, Crop, PET_correction)],
                                 by = c("Date", "Crop"), all.x = TRUE)
## There are fallows and long harvest period for some crops, correct to 0.15
WaterBalance[, PET_correction := ifelse(is.na(PET_correction), 0.15, PET_correction)
             ][, PET_correction := PET * PET_correction] # calculate actual PET

## Define inputs 

# 2. value for Wt0 (water deficit at start time, also in mm)
# 3. value for Ws0 (water deficit top soil at start time, also in mm)
# 4. value for AWHC (available water holding capacity in mm)
# 5. value for AWHCs (available water holding capacity for the top soil in mm)
Ws0 <- NULL # Profile water deficit
Wt0 <- NULL # Surface layer water deficit 
AWHC <- max(DT_profile_simple$Profile)
AWHC_60cm <- AWHC/8*3
AWHCs <- AWHC/8 # hypothetical values - super close to the observed value
# DT_summariesed[variable ==1 ]$SW %>% max()

## Prepare the critical input values 
key <- c("Irrigation", "N_rate")
PAWC_Profile <- PAWC_depth(DT_summariesed)
PAWC_top20cm <- PAWC_depth(DT_summariesed, maxdepth = 1)
SWD_Profile_Wt0 <- SWD_depth(DT_summariesed)[, .SD[1], by = key
                                             ][order(mget(key))]
SWD_Profile_Ws0 <- SWD_depth(DT_summariesed, maxdepth = 1
                             )[, .SD[1], by = key
                               ][order(mget(key))]
Deficit <- merge.data.table(SWD_Profile_Wt0[,.(Irrigation, N_rate, SWD)],
                            SWD_Profile_Ws0[,.(Irrigation, N_rate, SWD)],
                            by = key, suffixes = c("Wt0","Ws0"))
AWHC <- merge.data.table(PAWC_Profile[,.(Irrigation, N_rate, AWHc)],
                         PAWC_top20cm[,.(Irrigation, N_rate, AWHc)],
                            by = key, suffixes = c("","s"))
WB_input <- merge.data.table(Deficit, AWHC, by = key)
## Subset the water balance input data frame
cmd <- paste0("WaterBalance[,.(Date, PET, Precipitation =  Precipitation.", rep(c(1,2), each = 4), ")]" )
cmd <- paste0("list(", paste(cmd, collapse = ", "),")")

WB_input[, wbDT := eval(parse(text = cmd))]

wb_list <- vector("list",length = nrow(WB_input))
for(i in 1:nrow(WB_input)){
 wb_list[[i]] <-  ScotterWaterbalance(WB_input$wbDT[[i]], 
                                      Wt0 = WB_input$SWDWt0[i], 
                                      Ws0 = WB_input$SWDWs0[i], 
                                      AWHC = WB_input$AWHc[i],
                                      AWHCs = WB_input$AWHcs[i], 
                                      reset = FALSE)
  
}
WB_input[, wb:= wb_list]
wb <- WB_input[,  unlist(wb, recursive = FALSE), by = key]

update_WaterBalance.1 <- dcast.data.table(wb[Irrigation == 1], Date ~ N_rate, 
                                          value.var = c("Wt", "Drainage"))
update_WaterBalance.1 <- WaterBalance[, .(Date, PET, Precipitation = Precipitation.1)
                                      ][update_WaterBalance.1, on = "Date"]

update_WaterBalance.1 <- copy(profile_simpleSWD.irr1)[, Crop:= NULL
                                                     ][update_WaterBalance.1, on = "Date"]
update_WaterBalance.2 <- dcast.data.table(wb[Irrigation == 2], Date ~ N_rate, 
                                          value.var =  c("Wt", "Drainage"))
update_WaterBalance.2 <- WaterBalance[, .(Date, PET, Precipitation = Precipitation.2)
                                      ][update_WaterBalance.2, on = "Date"]
update_WaterBalance.2 <- copy(profile_simpleSWD.irr2)[, Crop:= NULL
                                                     ][update_WaterBalance.2, on = "Date"]

# 60cm --------------------------------------------------------------------

PAWC_Profile_60cm <- PAWC_depth(DT_summariesed, maxdepth = 3)
SWD_Profile_Wt0_60cm <- SWD_depth(DT_summariesed, 
                                  maxdepth = 3)[,.SD[1], by = key
                                                ][order(mget(key))]
Deficit_60cm <- merge.data.table(SWD_Profile_Wt0_60cm[,.(Irrigation, N_rate, SWD)],
                                 SWD_Profile_Ws0[,.(Irrigation, N_rate, SWD)],
                                 by = key, suffixes = c("Wt0","Ws0"))
AWHC_60cm <- merge.data.table(PAWC_Profile_60cm[,.(Irrigation, N_rate, AWHc)],
                              PAWC_top20cm[,.(Irrigation, N_rate, AWHc)],
                              by = key, suffixes = c("","s"))
WB_input_60cm <- merge.data.table(Deficit_60cm, AWHC_60cm, by = key)
## Subset the water balance input data frame
cmd <- paste0("WaterBalance[,.(Date, PET, Precipitation =  Precipitation.", rep(c(1,2), each = 4), ")]" )
cmd <- paste0("list(", paste(cmd, collapse = ", "),")")

WB_input_60cm[, wbDT := eval(parse(text = cmd))]

# PET correction  ---------------------------------------------------------
## The actual measurements for reset the water balance model 
reset_df <- SWD_depth(DT_summariesed, maxdepth = 3)
## Nested into a list column for easy looping through
reset_df <- reset_df[, list(realSWD = list(.SD)), by = .(Irrigation, N_rate)]
## Have commands to assemble a table with nested column
cmd <- paste0("WaterBalance[,.(Date, PET = PET_correction, Precipitation =  Precipitation.", rep(c(1,2), each = 4), ")]" )
cmd <- paste0("list(", paste(cmd, collapse = ", "),")")
## add new column to 60cm table 
WB_input_60cm[, wbDT_reset_PETcor := eval(parse(text = cmd))]
## Create a list for compute the soil water balance 
wb_list_60cm_reset_PETcor <- vector("list",length = nrow(WB_input_60cm))
## Compute the balance 
for(i in 1:nrow(WB_input)){
  wb_list_60cm_reset_PETcor[[i]] <-  ScotterWaterbalance(WB_input_60cm$wbDT_reset_PETcor[[i]], 
                                                         Wt0 = WB_input_60cm$SWDWt0[i], 
                                                         Ws0 = WB_input_60cm$SWDWs0[i], 
                                                         AWHC = WB_input_60cm$AWHc[i],
                                                         AWHCs = WB_input_60cm$AWHcs[i],
                                                         reset = TRUE,
                                                         reset_df = reset_df$realSWD[[i]])
}
## Save the results back and overwrite a column for subsequent processes
WB_input_60cm[, wb_reset:= wb_list_60cm_reset_PETcor]
## Unnest the column
wb_60cm_reset_PETcor <- WB_input_60cm[,  unlist(wb_reset, recursive = FALSE), by = key]

update_WaterBalance.1_60cm_reset_PETcor <- dcast.data.table(wb_60cm_reset_PETcor[Irrigation == 1], Date ~ N_rate,
                                                     value.var = c("Wt",  "SWD","Drainage"))
update_WaterBalance.1_60cm_reset_PETcor <- WaterBalance[, Date := as.Date(Date)
                                                        ][, .(Date, PET, Precipitation = Precipitation.1)
                                                          ][update_WaterBalance.1_60cm_reset_PETcor, on = "Date"]

update_WaterBalance.1_60cm_reset_PETcor <- copy(update_simpleSWD.irr1)[, ':='(Crop = NULL,
                                                                       Date = as.Date(Date))
                                                                       ][update_WaterBalance.1_60cm_reset_PETcor, on = "Date"]
update_WaterBalance.2_60cm_reset_PETcor <- dcast.data.table(wb_60cm_reset_PETcor[Irrigation == 2], Date ~ N_rate,
                                                            value.var =  c("Wt", "Drainage"))
update_WaterBalance.2_60cm_reset_PETcor <- WaterBalance[, Date := as.Date(Date)
                                                        ][, .(Date, PET, Precipitation = Precipitation.2)
                                                          ][update_WaterBalance.2_60cm_reset_PETcor, on = "Date"]
update_WaterBalance.2_60cm_reset_PETcor <- copy(update_simpleSWD.irr2)[, ':='(Crop = NULL,
                                                                       Date = as.Date(Date))
                                                                       ][update_WaterBalance.2_60cm_reset_PETcor, on = "Date"]


