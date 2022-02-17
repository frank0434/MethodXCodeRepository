
# Aim:
# grab either raw or processed data and upload to the db

source("Scripts/credentials.R")
con = dbConnect(PostgreSQL(), 
                host = dbcreds$host,
                dbname  = dbcreds$dbname,
                user = dbcreds$user,
                password = dbcreds$password)

dbListTables(con)
waitingForUpdate <- grep("update", ls(), value = TRUE)

lapply(waitingForUpdate, function(x){
  dbWriteTable(con, name = x, value = get(x), overwrite = TRUE, row.names = FALSE)
  # sql <- paste0('ALTER TABLE "', x, '" ADD CONSTRAINT "', x, '.N_rate" PRIMARY KEY ("N_rate");')
  # dbExecute(con, sql)
  
})
dbDisconnect(con)
