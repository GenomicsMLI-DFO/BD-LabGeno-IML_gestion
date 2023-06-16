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

  cat(crayon::green("\nLooking that new data are not already within the DB, DNA version!\n"))

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

check_key_ADNe <- function(data, DB = "LabGeno"){

  cat(crayon::green("\nLooking that new data are not already within the DB, eDNA version!\n"))

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

  # SITES_ADNE

  check_key_unique(data = data, DB = DB,
                   table = "Sites_ADNe",
                   DB.table = "21_Sites_ADNe",
                   key = "Numero_unique_site_ADNe")

  # STATION_ADNE

  check_key_unique(data = data, DB = DB,
                   table = "Stations_ADNe",
                   DB.table = "22_Stations_ADNe",
                   key = "Numero_unique_station_ADNe")


  # ECHANTILLON_ADNE

  check_key_unique(data = data, DB = DB,
                   table = "Echantillons_ADNe",
                   DB.table = "23_Echantillons_ADNe",
                   key = "Numero_unique_echantillon_ADNe")

  # Filtre

  check_key_unique(data = data, DB = DB,
                   table = "Filtres_ADNe",
                   DB.table = "24_Filtres_ADNe",
                   key = "Numero_unique_filtre_ADNe")

  # Extrait

  check_key_unique(data = data, DB = DB,
                   table = "Extraits_ADNe",
                   DB.table = "25_Extraits_ADNe",
                   key = "Numero_unique_extrait_ADNe")

  # QPCR

  check_key_unique(data = data, DB = DB,
                   table = "qPCR_ADNe",
                   DB.table = "26c_qPCR_ADNe",
                   key = "ID_qPCR_ADNe")

  # COURBE

  check_key_unique(data = data, DB = DB,
                   table = "Courbe_etalonnage_ADNe",
                   DB.table = "26a_Courbe_etalonnage_ADNe",
                   key = "Courbe_ID")

  # Purification_librairies_ADNe

  check_key_unique(data = data, DB = DB,
                   table = "Purification_librairies_ADNe",
                   DB.table = "28a_Purification_librairies_ADNe",
                   key = "Numero_unique_librairie_ADNe")

  # Analyses_externes_librairies_ADNe

  check_key_unique(data = data, DB = DB,
                   table = "Analyses_externes_librairies_ADNe",
                   DB.table = "28b_Analyses_externes_librairies",
                   key = "Numero_unique_librairie_SeqReady_ADNe")


} # END of the function




#' @title Internal function to correct key column by column
#'
#' @description
#' Function to correct some weird format of latitude and longitude
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data a string
#' @param DB a string
#' @param table a string
#' @param DB.table a string
#' @param key a string
#'
#' @examples
#'
#' @export

check_key_unique <- function(data, DB, table, DB.table, key){

  # SITES_ADNE
  if(c(table) %in% names(data)  ){

    cat("\nImporting", crayon::cyan(table), "KEY:\n")

    Numero_unique   <- data[[table]][[key]]

    DB.key <- load_columns_DB(columns = key,
                              table = DB.table, DB = DB, verbose = F)
    DB.key <- DB.key[[key]]

    commun <- NULL
    commun <- base::intersect(Numero_unique, DB.key)

    cat("\n", length(Numero_unique), "new", key, "detected\n")

    if(length(commun) > 0){
      cat(crayon::red("\n", length(commun), "keys are already present in the DB:", paste(commun, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    } else{
      cat(crayon::green("\nNo new",key, "already present in the DB, it's perfect!\n"))
    }
  }

}


