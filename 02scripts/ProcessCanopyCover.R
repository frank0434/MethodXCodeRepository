source("C:/Data/SVS/02scripts/packages.R")
source("C:/Data/SVS/02scripts/simpleSWD.R")
## Too much effort to build a webscraper assessmble file names to extrat
base_url <- "https://iplant.plantandfood.co.nz/project/I190710/DataProtocols/"
crops <- c("Onion", "Onions","Wheat", "Broccoli","Potato")
tech <- c("Sunscan", "Greenseeker")
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
  dt <- as.data.table(read_excel(path = x, skip = 5, sheet = "NDVI raw data"))
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
