############ GENERAL/HELPER/MISCELLANEOUS FUNCTIONS ################


## this function closes all database connections
close_all_db_cons <- function() {
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) dbDisconnect(con)  
}


