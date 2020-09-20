
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
id_var <- c("Crop", "Date", "Plot_No","Irrigation...8", "N_rate", "Irrigation...18","Rainfall")

DT_long <- melt(DT, 
                id.vars = id_var, measure.vars = value_var, 
                variable.factor = FALSE)

DT_summariesed <- DT_long[, .(SW = mean(as.numeric(value), na.rm = TRUE)), 
                          by = c(id_var,"variable")
                          ]
# Cautious!!!!! SW is in percentage


DT_summariesed[order(Date,Plot_No, N_rate, Irrigation...8, decreasing = TRUE)]
DT_profile <- DT_summariesed[, .(Profile = sum(SW, na.rm = TRUE)), by = id_var]
update_profile <- DT_profile[,.(Profile_mean = mean(Profile)), 
                             by = .(Crop, Date, N_rate, Irrigation...8, Irrigation...18, Rainfall)
                             ][, ':='(Treatment = paste0("Nitrogen_", N_rate, ":", 
                                                         "Irrigation_", Irrigation...8),
                                      Date = as.POSIXct(Date, tz = "NZ"))]
# Transfer to wide to show in grafana
update_profile <- dcast(update_profile, 
                        Crop + Date + Rainfall + Irrigation...18 ~ Treatment, value.var = "Profile_mean", )
## Update the name 
setnames(update_profile, "Irrigation...18", "Irrigation")


# Soil water deficit

deficit <- grep("^Nitrogen.+", colnames(update_profile), value = TRUE)
update_profile <- update_profile[, (paste0("deficit", deficit)) := lapply(.SD, function(x){
  # Split the vector into different groups 
  vv <- split(x, cumsum(!is.na(x)))
  # Replace NA with the first non NA value
  vv <- sapply(vv, function(x) {
    sapply(x,function(xx){
      ifelse(is.na(xx), x[1], xx)
    })
  })
  # Unlist the vector
  vv <- unlist(vv)
  # Calculate the lag one difference 
  x <- c(0, diff(na.omit(vv), lag = 1))
  }), by = .(Crop), .SDcols = deficit][]

# Soil water balance
source("02scripts/ScotterWaterbalance.R")
## Joining PET and precipitation
WaterBalance <- merge(PET, update_profile[,.(Date = as.Date(Date), Rainfall, Irrigation)], by = 'Date', all.x  = TRUE)
# Replace NA with 0 to do calculation
WaterBalance[, ':='(Rainfall = ifelse(is.na(Rainfall), 0, Rainfall),
                    Irrigation = ifelse(is.na(Irrigation), 0, Irrigation))
             ][,Precipitation:=Rainfall+Irrigation]

## Define inputs 

Ws0 <- -31
Wt0 <- -41
AWHC <- 280
AWHCs <- 31
# 2. value for Wt0 (water deficit at start time, also in mm)
# 3. value for Ws0 (water deficit top soil at start time, also in mm)
# 4. value for AWHC (available water holding capacity in mm)
# 5. value for AWHCs (available water holding capacity for the top soil in mm)

update_waterbalance <- ScotterWaterbalance(WaterBalance, Wt0, Ws0 = Ws0, AWHC = AWHC, AWHCs = AWHCs)
update_waterbalance <- update_waterbalance[, Date:=as.POSIXct(Date, tz = "NZ")] 


update_waterbalance %>% 
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
