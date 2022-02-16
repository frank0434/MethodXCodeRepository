### Based on equation from Scotter et. al . (1979) and the modified equations from Scotter and Horne (2016) the function calculates the modelled values for:
# - soil water deficit (W(t)) for the total planting zone (in mm)
# - soil water deficits for the top soil Ws(t) (in mm)
# - the factor Es(t) which describes the maximum amount of water which can be extracted from the top soil due to evapotranspiration(in mm)
# - the factor Ex (t) which describes the maximum amount of water that the soil in the total planting zone is capable of supplying for evapotranspiration (in mm)
# - actual evapotranspiration AET(t) (in mm)
# - Drainage (in mm)

#' ScotterWaterbalance
#'
#' @param weatherdata 
#' @param Wt0 
#' @param Ws0 
#' @param AWHC 
#' @param AWHCs 
#' @param reset 
#' @param reset_df 
#' 
#' @details 
#' Function input
#' 1. data frame which contains the values for PET (in mm) and Precipitation (Rain+Irrigation in mm). The columns have to be named as "PET" and "Precipitation". 
#'    In the function the data frame is called "weatherdata" but the df can be named differently as long as the columns are named as described above.
#' 2. value for Wt0 (water deficit at start time, also in mm)
#' 3. value for Ws0 (water deficit top soil at start time, also in mm)
#' 4. value for AWHC (available water holding capacity in mm)
#' 5. value for AWHCs (available water holding capacity for the top soil in mm)
#' 6. reset. logical. Declare whether or not to reset when new soil water content measurements are available. Default is TRUE. 
#' 7. reset_df. a data.frame or data.table contains the observation of soil water content measurements. 
#'    
#' @return
#' @export
#'
#' @examples
#' 
ScotterWaterbalance <- function(weatherdata, Wt0, Ws0, AWHC, AWHCs, 
                                reset = TRUE, reset_df = NULL){
  cols <- colnames(weatherdata)
  stopifnot("Precipitation" %in% cols)
  df_WaterBalance_outputs <- weatherdata[, Date := as.Date(Date)] 

  if(!is.null(reset_df)){
    reset_df$Date <- as.Date(reset_df$Date)
    df_WaterBalance_outputs <- merge(df_WaterBalance_outputs,
                                     reset_df, by = "Date", all.x = TRUE)
    
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
