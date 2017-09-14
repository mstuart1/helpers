# a function to connect to the Leyte database

conleyte <- function(){
  suppressMessages(library(dplyr))
  leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(leyte)
}
