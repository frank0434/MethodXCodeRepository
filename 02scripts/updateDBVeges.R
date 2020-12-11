source("02scripts/sourceDataVeges.R")
source("02scripts/ScotterWaterbalance.R")
source("02scripts/transderDataVeges.R")
# Aim:
# grab either raw or processed data and upload to the db


con = dbConnect(PostgreSQL(), 
                host = "database.powerplant.pfr.co.nz",
                dbname  = "cflfcl_Veges",
                user = "cflfcl_MPI_SVS",
                password = "CjSyST55uAU6t2cN" )

dbListTables(con)
waitingForUpdate <- grep("update", ls(), value = TRUE)

lapply(waitingForUpdate, function(x){
  dbWriteTable(con, name = x, value = get(x), overwrite = TRUE, row.names = FALSE)
  # sql <- paste0('ALTER TABLE "', x, '" ADD CONSTRAINT "', x, '.N_rate" PRIMARY KEY ("N_rate");')
  # dbExecute(con, sql)
  
})
dbDisconnect(con)
