
# Aim:
# Manipulate data 
source("02scripts/sourceData.R")

# required cols 
cols <- grep("^\\w", colnames(df), value = TRUE)

# Subset
DT <- df[!is.na(Crop), ..cols]

# Constant
colnames <- colnames(DT)
value_var <- grep("VWC.+", colnames, value = TRUE)
thickness <- 20L
id_var <- c("Crop", "Date", "Plot_No","Irrigation...8", "N_rate", "Rainfall")

DT_long <- melt(DT, 
                id.vars = id_var, measure.vars = value_var, 
                variable.factor = FALSE)

DT_summariesed <- DT_long[, .(SW = mean(as.numeric(value), na.rm = TRUE)), 
                          by = c(id_var,"variable")
                          ]
# Cautious!!!!! SW is in percentage

# 1.8 m profile
DT_summariesed[order(Date,Plot_No, N_rate, Irrigation...8, decreasing = TRUE)]


DT_profile <- DT_summariesed[, .(Profile = sum(SW, na.rm = TRUE)), by = id_var]

update_profile <- DT_profile[,.(Profile_mean = mean(Profile)), 
                             by = .(Crop, Date, N_rate, Irrigation...8,  Rainfall)
                             ][, ':='(Treatment = paste0("Nitrogen_", N_rate, ":", 
                                                         "Irrigation_", Irrigation...8),
                                      Date = as.POSIXct(Date, tz = "NZ"))]
# 0.6 m
DT_60cm <- copy(DT_summariesed)[variable %in% c("VWC_0.20", "VWC_20.40", "VWC_40.60")
                                ][, .(Profile = sum(SW, na.rm = TRUE)), 
                                  by = id_var
                                  ][,.(Profile_mean = mean(Profile)), 
                                    by = .(Crop, Date, N_rate, Irrigation...8,  Rainfall)
                                    ][, ':='(Treatment = paste0("Nitrogen_", N_rate, ":", 
                                                                "Irrigation_", Irrigation...8),
                                             Date = as.POSIXct(Date, tz = "NZ"))]

# Transfer to wide to show in grafana
update_profile <- dcast(update_profile, 
                        Crop + Date + Rainfall  ~ Treatment, value.var = "Profile_mean")
update_60cm <- dcast(DT_60cm, 
                     Crop + Date + Rainfall  ~ Treatment, value.var = "Profile_mean")
# Join the irrigation 

irrigation <- melt.data.table(df_irrigation, 
                              id.vars = c("Crop", "Date"), value.name = "Irrigation",
                              variable.factor = FALSE, variable.name = "Treatment" )
irrigation_sum <- irrigation[, .(Irrigation = sum(Irrigation, na.rm = TRUE)), 
                             by = .(Crop, Date)]

update_profile <- merge.data.table(update_profile, irrigation_sum, 
                                   by = c("Crop", "Date"), all.x = TRUE, all.y = TRUE)
update_60cm <- merge.data.table(update_60cm, irrigation_sum, 
                                by = c("Crop", "Date"), all.x = TRUE, all.y = TRUE)
# Soil water deficit

deficit <- grep("^Nitrogen.+", colnames(update_profile), value = TRUE)
update_profile <- update_profile[, (paste0("deficit", deficit)) := lapply(.SD, lagfun),
                                 by = .(Crop), .SDcols = deficit][]
update_60cm <- update_60cm[, (paste0("deficit", deficit)) := lapply(.SD, lagfun),
                           by = .(Crop), .SDcols = deficit][]


# Soil water balance
source("02scripts/ScotterWaterbalance.R")
## Joining PET and precipitation
WaterBalance <- merge(PET, update_profile[,.(Date = as.Date(Date), Rainfall, Irrigation)], by = 'Date', all.x  = TRUE)
WaterBalance_60cm <- merge(PET, update_60cm[,.(Date = as.Date(Date), Rainfall, Irrigation)], by = 'Date', all.x  = TRUE)

# Replace NA with 0 to do calculation
WaterBalance[, ':='(Rainfall = ifelse(is.na(Rainfall), 0, Rainfall),
                    Irrigation = ifelse(is.na(Irrigation), 0, Irrigation))
             ][,Precipitation:=Rainfall+Irrigation]
WaterBalance_60cm[, ':='(Rainfall = ifelse(is.na(Rainfall), 0, Rainfall),
                         Irrigation = ifelse(is.na(Irrigation), 0, Irrigation))
                  ][,Precipitation:=Rainfall+Irrigation]

## Define inputs 

Ws0 <- -31
Wt0 <- -41
AWHC <- 280
AWHC_60cm <- 93

AWHCs <- 31
# 2. value for Wt0 (water deficit at start time, also in mm)
# 3. value for Ws0 (water deficit top soil at start time, also in mm)
# 4. value for AWHC (available water holding capacity in mm)
# 5. value for AWHCs (available water holding capacity for the top soil in mm)

update_waterbalance <- ScotterWaterbalance(WaterBalance, Wt0, Ws0 = Ws0, AWHC = AWHC, AWHCs = AWHCs)
update_waterbalance <- update_waterbalance[, Date:=as.POSIXct(Date, tz = "NZ")] 
update_waterbalance_60cm <- ScotterWaterbalance(WaterBalance_60cm, Wt0, Ws0 = Ws0, AWHC = AWHC_60cm, AWHCs = AWHCs)
update_waterbalance_60cm <- update_waterbalance_60cm[, Date:=as.POSIXct(Date, tz = "NZ")] 


update_waterbalance %>% 
  ggplot(aes(Date)) +
  geom_point(aes(y = Wt)) +
  geom_line(aes(y= Wt)) +
  theme_classic()

update_waterbalance_60cm %>% 
  ggplot(aes(Date)) +
  geom_point(aes(y = Wt)) +
  geom_line(aes(y= Wt)) +
  theme_classic()

update_waterbalance[, ':='(CUMPET=cumsum(PET),
                           CUMWATERINPUT= cumsum(Precipitation),
                           CUMDrainage = cumsum(Drainage))]%>% 
  ggplot(aes(Date)) +
  geom_line(aes(y = CUMPET)) +
  geom_line(aes(y= CUMWATERINPUT), color = "blue") +
  geom_line(aes(y = CUMDrainage), color = "red")+
  theme_classic()
update_waterbalance_60cm[, ':='(CUMPET=cumsum(PET),
                           CUMWATERINPUT= cumsum(Precipitation),
                           CUMDrainage = cumsum(Drainage))]%>% 
  ggplot(aes(Date)) +
  geom_line(aes(y = CUMPET)) +
  geom_line(aes(y= CUMWATERINPUT), color = "blue") +
  geom_line(aes(y = CUMDrainage), color = "red")+
  theme_classic()
