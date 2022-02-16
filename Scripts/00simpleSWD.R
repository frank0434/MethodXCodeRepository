#' download_excel
#' @description This function downloads excel file from Microsoft SharePoint to a temp file. 
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
                           username = USERNAME, 
                           pass =  PASSWORD){
  httr::GET(url, authenticate(user = username, password = pass,
                        type = "ntlm"), 
      write_disk(tf <- tempfile(fileext = ".xlsx"), overwrite = TRUE))
  return(tf)
  
}


#' SWD_depth
#' @description Calculate the SWD in different profile. 
#' The function is customised to accommodating the project excel workbooks
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
#' 
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

#' Title
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
