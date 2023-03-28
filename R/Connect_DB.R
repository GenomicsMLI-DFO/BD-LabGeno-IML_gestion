#' @title Test connection with DB
#'
#' @description
#' Function to check that the connection with the DB is OK
#'
#' @details The connection to your DB should have been set previously on your computer with ODBC manager.
#'
#' @param DB Name of the OBDC database, as set in your computer.
#'
#' @examples
#' # Not to run
#' test_DB()
#'
#' @seealso [load_DB()] to upload a specific table
#'
#` @references
#' List references
#' @export

test_DB <- function(DB = "LabGeno"){

  con <- RODBC::odbcConnect(DB)

  if(is.null(attr(con, "connection.string"))){
   cat("\nSomething went wrong with the connection ...")
  } else {
  res <- sapply(stringr::str_split(attr(con, "connection.string"), ";"), `[`,2) |> stringr::str_remove("DBQ=")
   cat(paste("\nConnected to", res))

  }

  RODBC::odbcCloseAll()

} # End of function


#' @title Upload DB table
#'
#' @description
#' Function to upload one of the table in database
#'
#' @details
#' Possible tables are :  00_Liste_projets, 01_Groupes, 02_Specimens, 03_Tissus, 04_Extraits_ADN_ARN, 05_Analyse_Externe, 06_Sexage, 07_Sequencage, 08_Hormones, 09_GQ
#'
#' @param table Name of the table you want to load.
#' @param DB Name of the OBDC database, as set in your computer.
#'
#' @examples
#' # Not to run
#' res <-load_DB()
#' head(res)
#' @seealso
#'
#` @references
#' List references
#' @export

load_DB <- function(table = "00_Liste_projets", DB = "LabGeno"){

  con <- RODBC::odbcConnect(DB)

  if(is.null(attr(con, "connection.string"))){
    print("Something went wrong with the connection ...")
  } else {
    res <- sapply(stringr::str_split(attr(con, "connection.string"), ";"), `[`,2) |> stringr::str_remove("DBQ=")
    cat(paste("\nConnected to", res))

    export.df <-  sqlFetch(con, table)

    cat(paste("\nThe table", table, "was uploaded and contained", nrow(export.df), "entries.\n"))


  }

  RODBC::odbcCloseAll()

  return(export.df)

} # End of function


