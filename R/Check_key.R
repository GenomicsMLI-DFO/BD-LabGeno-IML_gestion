#' @title Check that we are not trying to import data that should be already present
#'
#' @description
#' Function to check that column name and order follow the predefined template
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data List containing important tables.
#' @param DB Name of the ODBC connection
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso [upload_gabarit_ADN()] to create the list of tables.
#'
#` @references
#' List references
#' @export

check_key <- function(data, DB = "LabGeno"){

  cat(crayon::green("\nLooking that new data are not already within the DB!\n"))

  # Establish the connection
  con <- RODBC::odbcConnect(DB)
  # Stop if you cannot connect
  if(is.null(attr(con, "connection.string"))){
    stop("Something went wrong with the connection ...")
    # Perform the function if you can connect
  }

  res <- sapply(stringr::str_split(attr(con, "connection.string"), ";"), `[`,2) |> stringr::str_remove("DBQ=")
  cat(paste("\nConnected to", res, "\n"))

  RODBC::odbcClose(con)

  # GROUPE
  if(c("Groupes") %in%  names(data)  ){

    cat("\nImporting", crayon::cyan("Groupes"), "KEY:\n")

    Numero_unique_groupe   <- data$Groupes$Numero_unique_groupe

    groupes.key <- load_columns_DB(columns = "Numero_unique_groupe", table = "01_Groupes", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_groupe)

    commun <- NULL
    commun <- base::intersect(Numero_unique_groupe, groupes.key)

    cat("\n", length(Numero_unique_groupe), "new Numero_unique_groupe detected\n")

    if(length(commun) > 0){
      cat(crayon::red("\n", length(commun), "keys are already present in the DB:", paste(commun, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    } else{
    cat(crayon::green("\nNo new Numero_unique_groupe already present in the DB, it's perfect!\n"))
    }
    }

  # SPECIMEN

  if(c("Specimens") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Specimens"), "KEY:\n")

    Numero_unique_specimen   <- data$Specimens$Numero_unique_specimen

    specimen.key <- load_columns_DB(columns = "Numero_unique_specimen", table = "02_Specimens", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_specimen)

    commun <- NULL
    commun <- base::intersect(Numero_unique_specimen, specimen.key)

    cat("\n", length(Numero_unique_specimen), "new Numero_unique_specimen detected\n")

    if(length(commun) > 0){
      cat(crayon::red("\n", length(commun), "keys are already present in the DB:", paste(commun, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }else{
      cat(crayon::green("\nNo new Numero_unique_specimen already present in the DB, it's perfect!\n"))
  }
}

  # TISSUS

  if(c("Tissus") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Tissus"), "KEY:\n")

    Numero_unique_tissu  <- data$Tissus$Numero_unique_tissu

    tissu.key <- load_columns_DB(columns = "Numero_unique_tissu", table = "03_Tissus", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_tissu)

    commun <- NULL
    commun <- base::intersect(Numero_unique_tissu, tissu.key)

    cat("\n", length(Numero_unique_tissu), "new Numero_unique_tissu detected\n")

    if(length(commun) > 0){
      cat(crayon::red("\n", length(commun), "keys are already present in the DB:", paste(commun, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    } else{
      cat(crayon::green("\nNo new Numero_unique_tissu already present in the DB, it's perfect!\n"))
    }
    }

  # EXTRAIT

  if(c("Extraits_ADN_ARN") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Extraits_ADN_ARN"), "KEY:\n")

    Numero_unique_extrait   <- data$Extraits_ADN_ARN$Numero_unique_extrait

    extrait.key <- load_columns_DB(columns = "Numero_unique_extrait", table = "04_Extraits_ADN_ARN", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_extrait)

    commun <- NULL
    commun <- base::intersect(Numero_unique_extrait, extrait.key)

    cat("\n", length(Numero_unique_extrait), "new Numero_unique_extrait detected\n")

    if(length(commun) > 0){
      cat(crayon::red("\n", length(commun), "keys are already present in the DB:", paste(commun, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }  else{
      cat(crayon::green("\nNo new Numero_unique_extrait already present in the DB, it's perfect!\n"))
  }}


} # END of the function
