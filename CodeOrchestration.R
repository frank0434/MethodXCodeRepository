# For example source("c:/<Path to the code>/ScotterWaterbalance.R")


Rcodes <- (list.files(path = "c:/Data/MethodXCodeRepository/Scripts/", 
                     pattern = "*.R", full.names = TRUE))

for(i in Rcodes){
  cat("Source R code", i, "\n")
  source(i)
}
  

