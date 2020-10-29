
# Aim:
# grab either raw or processed data and upload to the db


con = dbConnect(PostgreSQL(), 
                host = "database.powerplant.pfr.co.nz",
                dbname  = "cflfcl_MPI_SVS",
                user = "cflfcl_SVS",
                password = "vVsYDCzw7PD4Phb1" )

dbListTables(con)
waitingForUpdate <- grep("update", ls(), value = TRUE)

lapply(waitingForUpdate, function(x){
  dbWriteTable(con, name = x, value = get(x), overwrite = TRUE, row.names = FALSE)
  # sql <- paste0('ALTER TABLE "', x, '" ADD CONSTRAINT "', x, '.N_rate" PRIMARY KEY ("N_rate");')
  # dbExecute(con, sql)
  
})
dbDisconnect(con)
