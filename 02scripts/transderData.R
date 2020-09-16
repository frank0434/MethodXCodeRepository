
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


DT_summariesed[order(Date,Plot_No)]
DT_profile <- DT_summariesed[, .(Profile = sum(SW, na.rm = TRUE)), by = id_var]
DT_profile[,.(Profile_mean = mean(Profile)), by = .(Crop, Date, N_rate, Irrigation...8, Irrigation...18, Rainfall)]
