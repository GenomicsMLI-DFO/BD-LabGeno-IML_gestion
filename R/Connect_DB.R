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

    export.df <-  RODBC::sqlFetch(con, table)

    cat(paste("\nThe table", table, "was uploaded and contained", nrow(export.df), "entries.\n"))

  }

  RODBC::odbcCloseAll()

  return(export.df)

} # End of function

#' @title Upload specific column of a DB table
#'
#' @description
#' Function to upload one or more specific columns from a the table in database
#'
#' @details
#' Possible tables are :  00_Liste_projets, 01_Groupes, 02_Specimens, 03_Tissus, 04_Extraits_ADN_ARN, 05_Analyse_Externe, 06_Sexage, 07_Sequencage, 08_Hormones, 09_GQ
#'
#' @param columns Vector of the columne to upload.
#' @param table Name of the table you want to load.
#' @param DB Name of the OBDC database, as set in your computer.
#' @param verbose Print info if TRUE.
#'
#' @examples
#' # Not to run
#' res <-load_columns_DB(columns = c("Numero_unique_groupe"), table = "01_Groupes")
#' head(res)
#' @seealso
#'
#` @references
#' List references
#' @export



load_columns_DB <- function(columns, table, DB = "LabGeno", verbose = TRUE){

  if (missing(columns)| missing(table)) {
    stop("Please provide a value for both columns and table argument.")
  }

  # Establish the connection
  con <- RODBC::odbcConnect(DB)
  # Stop if you cannot connect
  if(is.null(attr(con, "connection.string"))){
    print("Something went wrong with the connection ...")
  # Perform the function if you can connect
  } else {

    res <- sapply(stringr::str_split(attr(con, "connection.string"), ";"), `[`,2) |> stringr::str_remove("DBQ=")

    if(verbose == T) cat(paste("\nConnected to", res))

    qry.1 <- paste("SELECT", paste(columns, sep = ", "), "FROM", paste0("[",table,"]"))

    export.df <- RODBC::sqlQuery(con, qry.1)

    if(verbose == T) cat(paste("\nThe columns from the table", table, "were uploaded and contained", nrow(export.df), "entries.\n"))

  }

  RODBC::odbcCloseAll()

  return(export.df)

}


#' @title List table and request contained in the database.
#'
#' @description
#' Function to ;ist possible main table and request contained in the database.
#'
#' @details The connection and deconnection to the DB is automatic
#'
#'
#' @param DB Name of the OBDC database, as set in your computer.
#'
#' @examples
#' list_DB()
#'
#' @seealso
#'
#` @references
#' List references
#' @export

list_DB <- function(DB = "LabGeno"){

  con <- RODBC::odbcConnect(DB)

  if(is.null(attr(con, "connection.string"))){
    print("Something went wrong with the connection ...")
  } else {
    res <- sapply(stringr::str_split(attr(con, "connection.string"), ";"), `[`,2) |> stringr::str_remove("DBQ=")
    cat(paste("\nConnected to", res))

    res <- RODBC::sqlTables(con) |> dplyr::filter(TABLE_TYPE %in% c("TABLE", "SYNONYM", "VIEW")) |>
           dplyr::select(TABLE_NAME, TABLE_TYPE)

    table.int <- res |> dplyr::filter(TABLE_TYPE %in% c("TABLE", "SYNONYM")) |> dplyr::pull(TABLE_NAME)
    request.int <- res |> dplyr::filter(TABLE_TYPE %in% c("TABLE", "VIEW")) |> dplyr::pull(TABLE_NAME)

    if(length(table.int) > 0){
      cat(paste("\n\nTables available are:",  paste(crayon::green(table.int), collapse = ", ")))
    } else{
      cat(crayon::red("\nNo table detected."))
    }


    if(length(request.int) > 0){
      cat(paste("\n\nPre-defined requests available are:",  paste(crayon::green(request.int), collapse = ", ")))
    } else{
      cat(crayon::red("\nNo pre-defined requests detected."))
    }

  }

  RODBC::odbcCloseAll()


} # End of function


