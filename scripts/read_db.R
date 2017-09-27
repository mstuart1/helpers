# read_db ####
#' access db for read-only purposes
#' @export
#' @name read_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- read_Db("Leyte")

read_db <- function(x){
  library(dplyr)
  db <- src_mysql(dbname = x, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(db)
}
