
# Aim:
# Manipulate data 
source("C:/Data/SVS/02scripts/sourceDataHB.R")
source("C:/Data/SVS/02scripts/ScotterWaterbalance.R")
# required cols 
cols <- grep("^\\w", colnames(df), value = TRUE)

# Subset
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
NAcells <- metadata[, Comments:=NULL][value == 1]

DT_long <- melt(DT, 
                id.vars = id_var, measure.vars = value_var, 
                variable.factor = FALSE)[, value := as.numeric(value)]
## only join the metadata if bad values oberserved 
if (isTRUE(nrow(NAcells) > 0)) {
  DTwithmeta <- merge.data.table(DT_long, NAcells, 
                                 by.x = c("Date", "Field_Plot_No", "variable"),
                                 by.y = c("Date", "Plot", "variable"), 
                                 all.x = TRUE, suffixes = c("", ".y"))
  DTwithmeta <- DTwithmeta[is.na(value.y)
                           ][, value.y := NULL]
  
} else {
    DTwithmeta <- DT_long
  }
# value is doubled to get the mm unit, `thickness` holds the converter. 
DT_summariesed <- DTwithmeta[, .(SW = mean(as.numeric(value)*thickness, na.rm = TRUE)), 
                             by = .(Crop, Date, variable, Irrigation...8, N_rate)]

# Transfer layer information to integer layers
layers_name <- unique(DT_summariesed$variable)

layers_no <- c(1, 5, 6, 7, 8, 2, 3, 4)

names(layers_no) <- layers_name
DT_summariesed$variable <- layers_no[DT_summariesed$variable]

# Simple SWD --------------------------------------------------------------
## Simple SWD uses a user-defined PAWC (usually the maximum value over a series measurement)
## SWD is calculated by subtracting the PAWC by the actual measurement

DT_profile_simple_60cm <- SWD_depth(DT_summariesed, maxdepth = 3)

update_simpleSWD.irr1 <- dcast.data.table(DT_profile_simple_60cm[Irrigation == 1],
                                          Crop + Date ~ N_rate, value.var = "SWD")
update_simpleSWD.irr2 <- dcast.data.table(DT_profile_simple_60cm[Irrigation == 2],
                                          Crop + Date ~ N_rate, value.var = "SWD")

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
                                                                  Date = as.POSIXct(Date, tz = "NZ"))]
# water balance -----------------------------------------------------------


WaterBalance <- copy(update_input)[, ':='(Irrigation.1 = ifelse(is.na(Irrigation.1), 0, Irrigation.1),
                                          Irrigation.2 = ifelse(is.na(Irrigation.2), 0, Irrigation.2))
                                   ][, ':='(Precipitation.1 = Rain + Irrigation.1,
                                            Precipitation.2 = Rain + Irrigation.2)]
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
                                             ][order(get(key))]
SWD_Profile_Ws0 <- SWD_depth(DT_summariesed, maxdepth = 1
                             )[, .SD[1], by = key
                               ][order(get(key))]
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
                                      AWHCs = WB_input$AWHcs[i])
  
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
                                                ][order(get(key))]
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

wb_list_60cm <- vector("list",length = nrow(WB_input_60cm))
for(i in 1:nrow(WB_input)){
  wb_list_60cm[[i]] <-  ScotterWaterbalance(WB_input_60cm$wbDT[[i]], 
                                       Wt0 = WB_input_60cm$SWDWt0[i], 
                                       Ws0 = WB_input_60cm$SWDWs0[i], 
                                       AWHC = WB_input_60cm$AWHc[i],
                                       AWHCs = WB_input_60cm$AWHcs[i])
  
}
WB_input_60cm[, wb:= wb_list_60cm]
wb_60cm <- WB_input_60cm[,  unlist(wb, recursive = FALSE), by = key]

update_WaterBalance.1_60cm <- dcast.data.table(wb_60cm[Irrigation == 1], Date ~ N_rate, 
                                               value.var = c("Wt", "Drainage"))
update_WaterBalance.1_60cm <- WaterBalance[, .(Date, PET, Precipitation = Precipitation.1)
                                           ][update_WaterBalance.1_60cm, on = "Date"]

update_WaterBalance.1_60cm <- copy(update_simpleSWD.irr1)[, Crop:= NULL
                                                          ][update_WaterBalance.1_60cm, on = "Date"]
update_WaterBalance.2_60cm <- dcast.data.table(wb_60cm[Irrigation == 2], Date ~ N_rate, 
                                               value.var =  c("Wt", "Drainage"))
update_WaterBalance.2_60cm <- WaterBalance[, .(Date, PET, Precipitation = Precipitation.2)
                                           ][update_WaterBalance.2_60cm, on = "Date"]
update_WaterBalance.2_60cm <- copy(update_simpleSWD.irr2)[, Crop:= NULL
                                                          ][update_WaterBalance.2_60cm, on = "Date"]

