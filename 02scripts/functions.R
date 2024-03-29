
# 1 ------------
#' download_excel
#' @description This function downloads excel file from iplant to a temp file. 
#'
#' @param url a string of url. The url needs to be like:
#' "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/SVS_PotatoOnion_SoilWater.xlsx"
#'
#' @details Iplant authenticate type is `ntlm`
#' 
#' @return
#' @export
#' @import httr
#'
#' @examples
#' 
download_excel <- function(url, 
                           username = "cflfcl", 
                           pass =  Sys.getenv("PASSWORD")){
  httr::GET(url, authenticate(user = username, password = pass,
                              type = "ntlm"), 
            write_disk(tf <- tempfile(fileext = ".xlsx"), overwrite = TRUE))
  return(tf)
  
}



# 2 ------------
# connect to database 

#' connect_upload
#' @description connect to postgresql db and upload named data frames into db(s)
#' 
#' @param host 
#' @param dbname 
#' @param user 
#' @param password 
#' @param waitingForUpdate 
#'
#' @return
#' @export
#'
#' @examples
connect_upload <- function(host = "database.powerplant.pfr.co.nz",
                           dbname = Sys.getenv("USERNAME"), 
                           user = Sys.getenv("USERNAME"),
                           password = Sys.getenv("PASSWORD"), 
                           waitingForUpdate, tab_name){
  stopifnot(!is.null(waitingForUpdate))
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                        host = host, dbname = dbname, 
                        user = user, password = password)
  DBI::dbListTables(con)
  
  DBI::dbWriteTable(con, name = tab_name, value = waitingForUpdate, 
                    overwrite = TRUE, row.names = FALSE)
  
  DBI::dbDisconnect(con)
  
  
}


# 3 ------------

#' retrieve_met
#' @description retrieve rain and PET data from NIWA (New Zealand) station. 
#' 
#' @param agent_number integer. default is Lincoln, New Zealand
#' @param start date object. must be yyyy-mm-dd
#' @param username character. username for downloading data from cliflo
#' @param password character. password for downloading data from cliflo
#'
#' @return
#' @export
#' 
#' @import clifro
#'
#' @examples
retrieve_met <- function(agent_number = 17603L, start,
                         username = NULL, password = NULL){
  if(is.null(username)){
    me <- cf_user(Sys.getenv("clifro_usr"),
                  Sys.getenv("clifro_pass"))
  } else{
    me <-cf_user(username, password)
  }
  my.dts <- cf_datatype(select_1 =     c(9, 3),
                        select_2 =     c(1, 1),
                        check_box = list(4, 1),
                        combo_box =    c(NA, NA))
  my.station <- cf_station(agent_number)
  cf.datalist <- cf_query(user = me,
                          datatype = my.dts,
                          station = my.station,
                          start_date = paste(start, "00"),
                          end_date = Sys.Date())
  return(cf.datalist)
}


# 4 ------------

## Too much effort to build a webscraper assessmble file names to extrat
# base_url <- "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/"
# crops <- c("Onion", "Onions","Wheat", "Broccoli","Potato")
# tech <- c("Sunscan", "Greenseeker")
#' retrieve_canopy
#' @description hard code download all canopy data and interpolated canopy 
#' development based on time. 
#'
#' @param base_url 
#' @param crops 
#' @param tech 
#'
#' @return
#' @export
#' @import httr, data.table
#'
#' @examples
retrieve_canopy <- function(base_url, crops, tech){
  filepath <- as.data.table(expand.grid("SVSLincoln", crops, tech))
  # filepath <- rbindlist(list(filepath, list("SVSLincoln", "Potato", "Greenseeker")))
  filepath[, paths := paste0(base_url, Var1, "_", Var2, "_",Var3, ".xlsx")]
  filepath
  l <- sapply(filepath$paths, function(x){
    r <- httr::GET(x, authenticate(user = Sys.getenv("USERNAME"), 
                                   password = Sys.getenv("PASSWORD"),
                                   type = "ntlm"), write_memory())
    status_code <- r$status_code
    cat(x, status_code, "\n")
    if(status_code == 200){
      tf <- download_excel(x)
      return(tf)
    } else {
      NA
    }
    
  }, simplify = TRUE)
  filepath[, filepaths:= l]
  data_gs <- filepath[!is.na(filepaths) & (Var3 == "Greenseeker")
  ][, data:=lapply(filepaths, function(x){
    dt <- as.data.table(read_excel(path = x, 
                                   skip = 5, sheet = "NDVI raw data"))
  })]
  ## Aggregate the raw to mean
  data_gs[, summary := lapply(data, function(x){
    # SELECT THE RIGHT COLS
    DT <- x[!is.na(Irrigation),.(Date, Irrigation, Nitrogen, NDVISC)
    ][, .(mean = mean(NDVISC, na.rm = TRUE),
          sd = sd(NDVISC, na.rm = TRUE)), # SUMMARISING
      by = .(Date, Irrigation, Nitrogen)]
    DT[, ':='(Date = as.Date(Date),
              Irrigation = as.character(Irrigation),
              Nitrogen = as.character(Nitrogen))]
  })]
  
  update_canopy <- data_gs[, unlist(summary, recursive = FALSE), by = .(Var1, Var2, Var3)]
  # Manually determine the date of full canopy cover
  ## There will be 4 values on that day, but that's ok, we only need the date and 
  ## the rough indication of the value 
  full_canopy <- update_canopy[mean>= 0.75, .SD[1], by = .(Var2)]
  
}


# 5 ------------
#' canopy_cover
#' @description manually fill the canopy coverage.
#' @param DT 
#'
#' @return
#' @export
#'
#' @examples
canopy_cover <- function(DT){
  
  DT <- DT[, ':='(Crop = zoo::na.locf(Crop), # forward fill
                  Events = zoo::na.locf(Events), # forward fill
                  PET_correction = fcase(mean %in% c(0, 0.1), 0.15,
                                         mean > 0.75, 1))]
  ### Need end value for interpolation in each group
  loc_last_in_group <- DT[, .I[.N], by = .(Events, Crop)]
  ### First value in each group will be the end value for the previous group
  ### Logic not working since the values not aligned - hard code the last values 
  DT[loc_last_in_group$V1, PET_correction := c(rep(c(0.15,1,1), 3),0.15)] # Fix this 
  ## update the interpolation 
  DT[, PET_correction:= zoo::na.approx(PET_correction, Date, na.rm = FALSE), 
     by = .(Crop, Events)]
  
  return(DT)
}

# 6 ------------
#' join_wb
#'
#' @param wb 
#' @param WaterBalance 
#' @param profile_simpleSWD.irr 
#'
#' @return
#' @export
#'
#' @examples
join_wb <- function(wb, WaterBalance, profile_simpleSWD.irr){
  update_WaterBalance.1 <- dcast.data.table(wb[Irrigation == 1], Date ~ N_rate,
                                            value.var = c("Wt", "Drainage"))
  update_WaterBalance.1 <- WaterBalance[, .(Date, PET, Precipitation = Precipitation.1)
                                        ][update_WaterBalance.1, on = "Date"]

  update_WaterBalance.1 <- copy(profile_simpleSWD.irr)[, Crop:= NULL
                                                        ][update_WaterBalance.1, on = "Date"]
  return(update_WaterBalance.1)
  
}

# 7 ------------
#' wb_simple
#'  @description Simple SWD uses a user-defined PAWC (usually the maximum value
#over a series measurement) # SWD is calculated by subtracting the PAWC by the
#actual measurement
#'
#' @param DT_summarised 
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
wb_simple <- function(DT_summarised){
  ## Empty list to store things
  l <- vector("list", length = 3)
  Profiles <- paste(c(20, 40, 60), "cm")
  for (i in seq_len(3)){
    DT_profile_simple_60cm <- SWD_depth(DT_summarised, maxdepth = i)
    l[[i]] <- DT_profile_simple_60cm[, profile := Profiles[i]]
  }
  DT_profile_simple_60cm <- data.table::rbindlist(l)
  return(DT_profile_simple_60cm)
}



# 8 ------------
#' wb_daily
#'
#' @param WaterBalance 
#' @param DT_summarised 
#' @param DT_profile_simple 
#' @param maxdepth 
#' @param reset 
#'
#' @return
#' @export
#'
#' @examples
wb_daily <- function(WaterBalance,
                     DT_profile_simple, 
                     DT_summarised,
                     maxdepth = 1, reset = FALSE){
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
  # DT_summarised[variable ==1 ]$SW %>% max()
  
  ## Prepare the critical input values 
  key <- c("Irrigation", "N_rate")
  PAWC_Profile <- PAWC_depth(DT_summarised)
  PAWC_topXcm <- PAWC_depth(DT_summarised, maxdepth)
  SWD_Profile_Wt0 <- SWD_depth(DT_summarised)[, .SD[1], by = key
                                               ][order(mget(key))]
  SWD_Profile_Ws0 <- SWD_depth(DT_summarised, maxdepth )[, .SD[1], 
                                                             by = key
                                                             ][order(mget(x = key))]
  Deficit <- merge.data.table(SWD_Profile_Wt0[,.(Irrigation, N_rate, SWD)],
                              SWD_Profile_Ws0[,.(Irrigation, N_rate, SWD)],
                              by = key, suffixes = c("Wt0","Ws0"))
  AWHC <- merge.data.table(PAWC_Profile[,.(Irrigation, N_rate, AWHc)],
                           PAWC_topXcm[,.(Irrigation, N_rate, AWHc)],
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
  return(wb)
}

# 9 ------------
#' wb_correction
#' @description merge wb with canopy and pet correction
#' @param WaterBalance_correction 
#' @param DT_canopy_correction 
#'
#' @return
#' @export
#'
#' @examples
wb_correction <- function(WaterBalance_correction, DT_canopy_correction){
  WaterBalance <- merge.data.table(WaterBalance_correction, 
                                   DT_canopy_correction[,.(Date, Crop, PET_correction)],
                                   by = c("Date", "Crop"), all.x = TRUE)
  ## There are fallows and long harvest period for some crops, correct to 0.15
  WaterBalance[, PET_correction := ifelse(is.na(PET_correction), 0.15, PET_correction)
               ][, PET_correction := PET * PET_correction] # calculate actual PET
  return(WaterBalance)

}


# 10 ------------

#' order_layer
#' @description hard code re-order. careful about the layer number and name. 
#' 
#' @param DT_summarised 
#' @param layers_no 
#' @param layers_name 
#'
#' @return
#' @export
#'
#' @examples
order_layer <- function(DT_summarised){
  # Transfer layer information to integer layers
  layers_name <- unique(DT_summarised$variable)
  ## Hard code layer
  layers_no <- c(1, 6, 7, 8, 2, 3, 4,5)
  names(layers_no) <- layers_name
  DT_summarised$variable <- layers_no[DT_summarised$variable]
  return(DT_summarised)
}



# 11 ------------
#' SWD_depth
#' @description Calculate the SWD in different profile. 
#'
#' @param DT a data.table. Depth is described by incremental layers of measurement
#' @param maxdepth an integer. The number one use to calculate the max depth.
#' @param PAWC a numeric value to state how much water the soil can hold.
#'   Default is NULL - maximum profile water value will be used as PAWC
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
SWD_depth <- function(DT, colname = "variable", maxdepth = 8, PAWC = NULL){
  if(!"SW"%in% colnames(DT)){
    print("Soil water value must be in column: SW.")
  }
  no.oflayers <- length(unique(DT[[colname]]))
  if(no.oflayers < maxdepth) {
    cat("You ask too much. I don't have values down to", maxdepth, "\r\n")
    }
  DT_profile <- DT[variable %in% seq(1, maxdepth)
                               ][,.(Profile = sum(SW, na.rm = TRUE)),
                                 by = .(Crop, Date, Irrigation...8, N_rate)]
  if(is.null(PAWC)){
    DT_profile <- DT_profile[, PAWC := max(Profile), by = .(Irrigation...8, N_rate)
                       ][, ':='(SWD = Profile - PAWC,
                                Irrigation = Irrigation...8,
                                N_rate = paste0("Nitrogen ", N_rate),
                                Date = as.Date(Date, tz = "NZ"))]
  } else{
    DT_profile[, ':='(SWD = Profile - PAWC,
                      Irrigation = Irrigation...8,
                      N_rate = paste0("Nitrogen ", N_rate),
                      Date = as.Date(Date, tz = "NZ"))]
    }
  return(DT_profile)
}

# 12 ------------
#' PAWC_depth
#'
#' @param DT 
#' @param colname 
#' @param maxdepth 
#'
#' @return
#' @export
#'
#' @examples
PAWC_depth <- function(DT, colname = "variable", maxdepth = 8){
  if(!"SW"%in% colnames(DT)){
    print("Soil water value must be in column: SW.")
  }
  no.oflayers <- length(unique(DT[[colname]]))
  if(no.oflayers < maxdepth) {
    cat("You ask too much. I don't have values down to", maxdepth, "\r\n")
  }
  DT_profile <- DT[variable %in% seq(1, maxdepth)
                   ][,.(Profile = sum(SW, na.rm = TRUE)),
                     by = .(Crop, Date, Irrigation...8, N_rate)
                     ][,.(AWHc = max(Profile)), by = .(Irrigation...8, N_rate)
                       ][, ':='(Irrigation = Irrigation...8,
                                N_rate = paste0("Nitrogen ", N_rate))]
  return(DT_profile[order(Irrigation, N_rate)])
}



# 13 ------------
### Based on equation from Scotter et. al . (1979) and the modified equations from Scotter and Horne (2016) the function calculates the modelled values for:
# - soil water deficit (W(t)) for the total planting zone (in mm)
# - soil water deficits for the top soil Ws(t) (in mm)
# - the factor Es(t) which describes the maximum amount of water which can be extracted from the top soil due to evapotranspiration(in mm)
# - the factor Ex (t) which describes the maximum amount of water that the soil in the total planting zone is capable of supplying for evapotranspiration (in mm)
# - actual evapotranspiration AET(t) (in mm)
# - Drainage (in mm)



###Function input
# 1. data frame which contains the values for PET (in mm) and Precipitation (Rain+Irrigation in mm). The columns have to be named as "PET" and "Precipitation". 
#     In the function the data frame is called "weatherdata" but the df can be named differently as long as the columns are named as described above. 
# 2. value for Wt0 (water deficit at start time, also in mm)
# 3. value for Ws0 (water deficit top soil at start time, also in mm)
# 4. value for AWHC (available water holding capacity in mm)
# 5. value for AWHCs (available water holding capacity for the top soil in mm)
ScotterWaterbalance <- function(weatherdata, Wt0, Ws0, AWHC, AWHCs, 
                                reset = TRUE, reset_dt = NULL){
  cols <- colnames(weatherdata)
  stopifnot("Precipitation" %in% cols)
  df_WaterBalance_outputs <- weatherdata[, Date := as.Date(Date)] 
  
  if(!is.null(reset_dt)){
    dt <- reset_dt[, Date := as.Date(Date)]
    df_WaterBalance_outputs <- merge.data.table(df_WaterBalance_outputs,
                                                dt, by = "Date", all.x = TRUE)
    
  }
  
  
  df_WaterBalance_outputs$Ws <- Ws0 
  df_WaterBalance_outputs$Es <- 0 
  df_WaterBalance_outputs$Wt <- Wt0  
  df_WaterBalance_outputs$RAW <- 0  
  df_WaterBalance_outputs$Ex <- 0  
  df_WaterBalance_outputs$AET <- 0 
  df_WaterBalance_outputs$Drainage <- 0
  
  n <- nrow(weatherdata)
  
  
  for (t in 2:n){
    if(isTRUE(reset)){
      ## RESET STUFF 
      df_WaterBalance_outputs$Es[t] =   min(weatherdata$PET[t], (AWHCs+df_WaterBalance_outputs$Ws[t-1]))
      
      df_WaterBalance_outputs$Ws[t] =   min(0, (df_WaterBalance_outputs$Ws[t-1]+weatherdata$Precipitation[t]-df_WaterBalance_outputs$Es[t]))
      
      df_WaterBalance_outputs$RAW[t]= (0.0073*(weatherdata$PET[t]*(AWHC+df_WaterBalance_outputs$Wt[t-1])))
      
      df_WaterBalance_outputs$Ex[t]= max(df_WaterBalance_outputs$Es[t], (weatherdata$PET[t]*((AWHC+df_WaterBalance_outputs$Wt[t-1])/(AWHC-df_WaterBalance_outputs$RAW[t]))))
      
      df_WaterBalance_outputs$AET[t]=min(weatherdata$PET[t],df_WaterBalance_outputs$Ex[t])
      
      df_WaterBalance_outputs$Wt[t]= min(0, ifelse(!is.na(df_WaterBalance_outputs$SWD[t]), df_WaterBalance_outputs$SWD[t],
                                                   (df_WaterBalance_outputs$Wt[t-1]+weatherdata$Precipitation[t]-df_WaterBalance_outputs$AET[t])))
      
      df_WaterBalance_outputs$Drainage[t]=max(0,df_WaterBalance_outputs$Wt[t]+df_WaterBalance_outputs$Wt[t-1]+weatherdata$Precipitation[t]-df_WaterBalance_outputs$AET[t])
      
      
    } else {
      
      
      df_WaterBalance_outputs$Es[t] =   min(weatherdata$PET[t], (AWHCs+df_WaterBalance_outputs$Ws[t-1]))
      
      df_WaterBalance_outputs$Ws[t] =   min(0, (df_WaterBalance_outputs$Ws[t-1]+weatherdata$Precipitation[t]-df_WaterBalance_outputs$Es[t]))
      
      df_WaterBalance_outputs$RAW[t]= (0.0073*(weatherdata$PET[t]*(AWHC+df_WaterBalance_outputs$Wt[t-1])))
      
      df_WaterBalance_outputs$Ex[t]= max(df_WaterBalance_outputs$Es[t], (weatherdata$PET[t]*((AWHC+df_WaterBalance_outputs$Wt[t-1])/(AWHC-df_WaterBalance_outputs$RAW[t]))))
      
      df_WaterBalance_outputs$AET[t]=min(weatherdata$PET[t],df_WaterBalance_outputs$Ex[t])
      
      df_WaterBalance_outputs$Wt[t]= min(0, (df_WaterBalance_outputs$Wt[t-1]+weatherdata$Precipitation[t]-df_WaterBalance_outputs$AET[t]))
      
      df_WaterBalance_outputs$Drainage[t]=max(0,df_WaterBalance_outputs$Wt[t]+df_WaterBalance_outputs$Wt[t-1]+weatherdata$Precipitation[t]-df_WaterBalance_outputs$AET[t])
      
    }  
  }
  
  return(df_WaterBalance_outputs)  
  
}

# 14 ------------
#' change_tz
#' @description change the excel file time zone to NZ so align with the climate 
#' @param DT 
#' @param timezone 
#'
#' @return
#' @export
#'
#' @examples
change_tz <- function(DT, timezone = "NZ"){
  # HARD STOP if conditions not meet
  stopifnot(is.data.table(DT), "Date"%in% colnames(DT))
  DT[, Date := as.Date(Date, tz = "NZ")]
  return(DT)
}

# 15 -------------
#' lagfun
#' @description use with lappy to calculate the lag 1 difference
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' 
lagfun <- function(x){
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
}

