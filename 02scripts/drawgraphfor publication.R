con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                      host = "database.powerplant.pfr.co.nz",
                      dbname = "cflfcl_MPI_SVS", 
                      user = Sys.getenv("USERNAME"),
                      password = Sys.getenv("PASSWORD"))
library(DBI)
dbListTables(con)
library(dbplyr)
library(data.table)
library(magrittr)
dt <- dbplyr::db_collect(con = con,
                         sql = 'select * from "update_WaterBalance.1_60cm_reset_PETcor"') %>% 
  as.data.table()
dt
library(ggplot2)
long <- dt[,.(Date, profile, `Wt_Nitrogen 1`, `Wt_Nitrogen 2`, `Wt_Nitrogen 3`,
              `Wt_Nitrogen 4`, `SWD_Nitrogen 1`, `SWD_Nitrogen 2`, `SWD_Nitrogen 3`,
              `SWD_Nitrogen 4`)] %>% 
  melt(id.vars = c('Date','profile')) 
  
top60 <- long[!profile %in% c("20 cm", "40 cm")]

labels <- paste0("N", 1:4)

p <- top60[variable%like%"Wt_"] %>% 
  ggplot(aes(Date, value)) +
  geom_line(aes(linetype = variable), size = 0.8) +
  geom_point(data = top60[!variable%like%"Wt_"], aes(color = variable), size = 3) + 
  theme_light() + 
  theme(legend.key.width=unit(4,"line")) +
  scale_linetype_manual(name = "Nitrogen Treatments", 
                        values = c("solid", "longdash", "dotdash","twodash"),
                        labels = labels) +
  scale_color_manual(name = "Nitrogen Treatments",
                     values = c("red","blue", "green", "purple"),
                     labels = labels) +
  labs(y = "Soil Moisture Deficit (mm)")
ggsave("_targets/objects/fig3.png", dpi = 400, height = 5, width = 9)

FIG4DAT <- dt[,.(Date,PET, Precipitation, `Drainage_Nitrogen 1`, 
                 `Drainage_Nitrogen 2`, `Drainage_Nitrogen 3`, 
                 `Drainage_Nitrogen 4`, profile)]
cols <- c("PET", "Precipitation", "Drainage_Nitrogen 1", 
          "Drainage_Nitrogen 2", "Drainage_Nitrogen 3", 
          "Drainage_Nitrogen 4")
FIG4DAT[, (cols) := lapply(.SD, function(x) ifelse(x == 0, NA, x)),
        .SDcol = cols][]
FIG4DAT <- FIG4DAT[!profile %in% c("20 cm", "40 cm")][, profile:=NULL]

d <- FIG4DAT[, .(Date, `Drainage_Nitrogen 1`, 
                 `Drainage_Nitrogen 2`, `Drainage_Nitrogen 3`, 
                 `Drainage_Nitrogen 4`)] %>% 
  melt(id.vars = c("Date")) 


base_p <- FIG4DAT %>% 
  # melt(id.vars = c("Date", "PET", "Precipitation")) %>%
  ggplot(aes(Date)) +
  geom_col(aes(y = Precipitation,fill = "blue"), width= 3, alpha = 0.5) +
  geom_point(aes(y = PET, color = "grey50"), size = 2, alpha = 0.8) +
  scale_color_manual(name = "", values = c("grey50"), labels = "PET") +
  scale_fill_manual(name = "", values = c("blue"), labels = "Precipitation" ) +
  theme_light() + 
  theme(legend.key.width=unit(1,"line")) 

base_p +
  geom_point(data = d, aes(y = -value, shape = variable), color = "Red") +
  scale_shape_manual(name = "", values = 1:4, labels = paste0("N", 1:4, " Drainage")) +
  labs(y = "Moisture Changes (mm)")
  
ggsave("_targets/objects/fig4.png", dpi = 400, height = 5, width = 9)

  
  
  geom_point(data = top60[!variable%like%"Wt_"], aes(color = variable), size = 3) + 
  theme_light() + 
  theme(legend.key.width=unit(4,"line")) +
  scale_linetype_manual(name = "Nitrogen Treatments", 
                        values = c("solid", "longdash", "dotdash","twodash"),
                        labels = labels) +
  scale_color_manual(name = "Nitrogen Treatments",
                     values = c("red","blue", "green", "purple"),
                     labels = labels) +
  labs(y = "Soil Moisture Deficit (mm)")
ggsave("_targets/objects/fig4.png", dpi = 400, height = 5, width = 9)





WaterBalance
wb_60cm_reset_PETcor
wb <- wb_60cm_reset_PETcor
profile_simpleSWD.irr <- update_simpleSWD.irr1
