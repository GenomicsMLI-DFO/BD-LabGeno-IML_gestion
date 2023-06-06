#' @title Check relations between tables
#'
#' @description
#' Function to check that relationship between table are respected
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data List containing important tables.
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

check_relation  <- function(data){

 # GROUPE - only importation
  if(c("Groupes") %in%  names(data)  ){

    cat("\nImporting", crayon::cyan("Groupes"), "table KEY:\n")

    Numero_unique_groupe   <- data$Groupes$Numero_unique_groupe

    dup <- Numero_unique_groupe[duplicated(Numero_unique_groupe)]

    cat("\n", length(Numero_unique_groupe), "Numero_unique_groupe detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    } else{
      cat(crayon::red("\nNo Groupes table detected\n"))
      Numero_unique_groupe   <- NULL
    }

  # SPECIMEN : 1 check

  if(c("Specimens") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Specimens"), "table KEY:\n")

    Numero_unique_specimen   <- data$Specimens$Numero_unique_specimen

    dup <- Numero_unique_specimen[duplicated(Numero_unique_specimen)]

    cat("\n", length(Numero_unique_specimen), "Numero_unique_specimen detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with group

    if(!is.null(Numero_unique_groupe)){

      if(all(data$Specimens$Numero_unique_groupe %in% Numero_unique_groupe)){
        cat(crayon::green("\nAll the Numero_unique_groupe observed exist in the Groupes table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_groupe observed exist in the Groupes table.\n",


           paste(unique(data$Specimens$Numero_unique_groupe[!data$Specimens$Numero_unique_groupe %in% Numero_unique_groupe]), collapse = ", ")
           , "\nare missing\n"))
          }

    } else {
    cat(crayon::red("\nNumero_unique_groupe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo Specimens table detected\n"))
    Numero_unique_specimen   <- NULL
  }



  # TISSUS : 1 check

  if(c("Tissus") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Tissus"), "table KEY:\n")

    Numero_unique_tissu  <- data$Tissus$Numero_unique_tissu

    dup <- Numero_unique_tissu[duplicated(Numero_unique_tissu)]

    cat("\n", length(Numero_unique_tissu), "Numero_unique_tissus detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Tissus$Numero_unique_specimen %in% Numero_unique_specimen)){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table.\n",
                        paste(unique(data$Tissus$Numero_unique_specimen[!data$Tissus$Numero_unique_specimen %in% Numero_unique_specimen]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }

  } else{
    cat(crayon::red("\nNo Tissus table detected\n"))
    Numero_unique_tissu   <- NULL
  }

# EXTRAIT : 2 checks

  if(c("Extraits_ADN_ARN") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Extraits_ADN_ARN"), "table KEY:\n")

    Numero_unique_extrait   <- data$Extraits_ADN_ARN$Numero_unique_extrait

    dup <- Numero_unique_extrait[duplicated(Numero_unique_extrait)]

    cat("\n", length(Numero_unique_extrait), "Numero_unique_extrait detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Extraits_ADN_ARN$Numero_unique_specimen %in% Numero_unique_specimen)){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table.\n",
                        paste(unique(data$Extraits_ADN_ARN$Numero_unique_specimen[!data$Extraits_ADN_ARN$Numero_unique_specimen %in% Numero_unique_specimen]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with tissus

    if(!is.null(Numero_unique_tissu)){

      if(all(data$Extraits_ADN_ARN$Numero_unique_tissu %in% Numero_unique_tissu)){
        cat(crayon::green("\nAll the Numero_unique_tissu observed exist in the Specimen table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_tissu observed exist in the Specimen table.\n",
                        paste(unique(data$Extraits_ADN_ARN$Numero_unique_tissu[!data$Extraits_ADN_ARN$Numero_unique_tissu %in% Numero_unique_tissu]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_tissu could not be checked in this table\n"))
    }

  } else{
    cat(crayon::red("\nNo Extrait_ADN_ARN table detected\n"))
    Numero_unique_extrait   <- NULL
  }


  # AAnalyse_Externe : 2 checks

  if(c("Analyse_Externe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Analyse_Externe"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Analyse_Externe$Numero_unique_specimen %in% Numero_unique_specimen)){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table.\n",
                        paste(unique(data$Analyse_Externe$Numero_unique_specimen[!data$Analyse_Externe$Numero_unique_specimen %in% Numero_unique_specimen]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with extrai

    if(!is.null(Numero_unique_extrait)){

      if(all(data$Analyse_Externe$Numero_unique_extrait %in% Numero_unique_extrait)){
        cat(crayon::green("\nAll the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table.\n",
                        paste(unique(data$Analyse_Externe$Numero_unique_extrait[!data$Analyse_Externe$Numero_unique_extrait %in% Numero_unique_extrait]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extra could not be checked in this table\n"))
    }

  }



  # Sequencage : 2 checks

  if(c("Sequencage") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Sequencage"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Sequencage$Numero_unique_specimen %in% Numero_unique_specimen)){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table.\n",
                        paste(unique(data$Sequencage$Numero_unique_specimen[!data$Sequencage$Numero_unique_specimen %in% Numero_unique_specimen]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with extrai

    if(!is.null(Numero_unique_extrait)){

      if(all(data$Sequencage$Numero_unique_extrait %in% Numero_unique_extrait)){
        cat(crayon::green("\nAll the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table.\n",
                        paste(unique(data$Sequencage$Numero_unique_extrait[!data$Sequencage$Numero_unique_extrait %in% Numero_unique_extrait]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extrait could not be checked in this table\n"))
    }

  }



  # Sexage : 2 checks

  if(c("Sexage") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Sexage"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Sexage$Numero_unique_specimen %in% Numero_unique_specimen)){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table.\n",
                        paste(unique(data$Sexage$Numero_unique_specimen[!data$Sexage$Numero_unique_specimen %in% Numero_unique_specimen]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with extrai

    if(!is.null(Numero_unique_extrait)){

      if(all(data$Sexage$Numero_unique_extrait %in% Numero_unique_extrait)){
        cat(crayon::green("\nAll the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table.\n",
                        paste(unique(data$Sexage$Numero_unique_extrait[!data$Sexage$Numero_unique_extrait %in% Numero_unique_extrait]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extrait could not be checked in this table\n"))
    }

  }

  # Hormone : 1 check

  if(c("Hormones") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Hormones"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Hormones$Numero_unique_specimen %in% Numero_unique_specimen)){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table.\n",
                        paste(unique(data$Hormones$Numero_unique_specimen[!data$Hormones$Numero_unique_specimen %in% Numero_unique_specimen]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }

    }

} # END of the function


#' @title Check relations between tables, including a connection with the DB
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

check_relation_wDB  <- function(data, DB = "LabGeno"){

  cat(crayon::green("\nChecking relationship between tables\n"))

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

  groupes.key <- load_columns_DB(columns = "Numero_unique_groupe", table = "01_Groupes", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_groupe)
  specimen.key <- load_columns_DB(columns = "Numero_unique_specimen", table = "02_Specimens", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_specimen)
  tissu.key <- load_columns_DB(columns = "Numero_unique_tissu", table = "03_Tissus", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_tissu)
  extrait.key <- load_columns_DB(columns = "Numero_unique_extrait", table = "04_Extraits_ADN_ARN", DB = DB, verbose = F) |> dplyr::pull(Numero_unique_extrait)

  # GROUPE - only importation
  if(c("Groupes") %in%  names(data)  ){

    cat("\nImporting", crayon::cyan("Groupes"), "table KEY:\n")

    Numero_unique_groupe   <- data$Groupes$Numero_unique_groupe

    dup <- Numero_unique_groupe[duplicated(Numero_unique_groupe)]

    cat("\n", length(Numero_unique_groupe), "Numero_unique_groupe detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

  } else{
    cat(crayon::red("\nNo Groupes table detected\n"))
    Numero_unique_groupe   <- NULL
  }

  # SPECIMEN : 1 check

  if(c("Specimens") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Specimens"), "table KEY:\n")

    Numero_unique_specimen   <- data$Specimens$Numero_unique_specimen

    dup <- Numero_unique_specimen[duplicated(Numero_unique_specimen)]

    cat("\n", length(Numero_unique_specimen), "Numero_unique_specimen detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with group

    if(!is.null(Numero_unique_groupe)){

      if(all(data$Specimens$Numero_unique_groupe %in% c(Numero_unique_groupe, groupes.key))){
        cat(crayon::green("\nAll the Numero_unique_groupe observed exist in the Groupes table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_groupe observed exist in the Groupes table or the DB.\n",


                        paste(unique(data$Specimens$Numero_unique_groupe[!data$Specimens$Numero_unique_groupe %in% c(Numero_unique_groupe, groupes.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_groupe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo Specimens table detected\n"))
    Numero_unique_specimen   <- NULL
  }



  # TISSUS : 1 check

  if(c("Tissus") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Tissus"), "table KEY:\n")

    Numero_unique_tissu  <- data$Tissus$Numero_unique_tissu

    dup <- Numero_unique_tissu[duplicated(Numero_unique_tissu)]

    cat("\n", length(Numero_unique_tissu), "Numero_unique_tissus detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Tissus$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key))){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table or the DB.\n",
                        paste(unique(data$Tissus$Numero_unique_specimen[!data$Tissus$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }

  } else{
    cat(crayon::red("\nNo Tissus table detected\n"))
    Numero_unique_tissu   <- NULL
  }

  # EXTRAIT : 2 checks

  if(c("Extraits_ADN_ARN") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Extraits_ADN_ARN"), "table KEY:\n")

    Numero_unique_extrait   <- data$Extraits_ADN_ARN$Numero_unique_extrait

    dup <- Numero_unique_extrait[duplicated(Numero_unique_extrait)]

    cat("\n", length(Numero_unique_extrait), "Numero_unique_extrait detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Extraits_ADN_ARN$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key))){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table or the DB.\n",
                        paste(unique(data$Extraits_ADN_ARN$Numero_unique_specimen[!data$Extraits_ADN_ARN$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with tissus

    if(!is.null(Numero_unique_tissu)){

      if(all(data$Extraits_ADN_ARN$Numero_unique_tissu %in% c(Numero_unique_tissu, tissu.key))){
        cat(crayon::green("\nAll the Numero_unique_tissu observed exist in the Specimen table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_tissu observed exist in the Specimen table or the DB.\n",
                        paste(unique(data$Extraits_ADN_ARN$Numero_unique_tissu[!data$Extraits_ADN_ARN$Numero_unique_tissu %in% c(Numero_unique_tissu, tissu.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_tissu could not be checked in this table\n"))
    }

  } else{
    cat(crayon::red("\nNo Extrait_ADN_ARN table detected\n"))
    Numero_unique_extrait   <- NULL
  }


  # AAnalyse_Externe : 2 checks

  if(c("Analyse_Externe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Analyse_Externe"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Analyse_Externe$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key))){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table or the DB.\n",
                        paste(unique(data$Analyse_Externe$Numero_unique_specimen[!data$Analyse_Externe$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with extrai

    if(!is.null(Numero_unique_extrait)){

      if(all(data$Analyse_Externe$Numero_unique_extrait %in% c(Numero_unique_extrait, extrait.key))){
        cat(crayon::green("\nAll the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table or the DB.\n",
                        paste(unique(data$Analyse_Externe$Numero_unique_extrait[!data$Analyse_Externe$Numero_unique_extrait %in% c(Numero_unique_extrait, extrait.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extra could not be checked in this table\n"))
    }

  }



  # Sequencage : 2 checks

  if(c("Sequencage") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Sequencage"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Sequencage$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key))){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table or the DB.\n",
                        paste(unique(data$Sequencage$Numero_unique_specimen[!data$Sequencage$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with extrait

    if(!is.null(Numero_unique_extrait)){

      if(all(data$Sequencage$Numero_unique_extrait %in% c(Numero_unique_extrait, extrait.key))){
        cat(crayon::green("\nAll the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table or the DB.\n",
                        paste(unique(data$Sequencage$Numero_unique_extrait[!data$Sequencage$Numero_unique_extrait %in% c(Numero_unique_extrait, extrait.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extrait could not be checked in this table\n"))
    }

  }



  # Sexage : 2 checks

  if(c("Sexage") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Sexage"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Sexage$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key))){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table or the DB.\n",
                        paste(unique(data$Sexage$Numero_unique_specimen[!data$Sexage$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }


    # Check with extrai

    if(!is.null(Numero_unique_extrait)){

      if(all(data$Sexage$Numero_unique_extrait %in% c(Numero_unique_extrait, extrait.key))){
        cat(crayon::green("\nAll the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait observed exist in the Extrait_ADN_ARN table or the DB.\n",
                        paste(unique(data$Sexage$Numero_unique_extrait[!data$Sexage$Numero_unique_extrait %in% c(Numero_unique_extrait, extrait.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extrait could not be checked in this table\n"))
    }

  }

  # Hormone : 1 check

  if(c("Hormones") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Hormones"), "table KEY:\n")

    # Check with specimens

    if(!is.null(Numero_unique_specimen)){

      if(all(data$Hormones$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key))){
        cat(crayon::green("\nAll the Numero_unique_specimen observed exist in the Specimen table or the DB.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_specimen observed exist in the Specimen table or the DB.\n",
                        paste(unique(data$Hormones$Numero_unique_specimen[!data$Hormones$Numero_unique_specimen %in% c(Numero_unique_specimen, specimen.key)]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_specimen could not be checked in this table\n"))
    }

  }

} # END of the function


#' @title Check relations between tables for the eDNA gabarit
#'
#' @description
#' Function to check that relationship between table are respected
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data List containing important tables.
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

check_relation_ADNe  <- function(data){

  # Sites_ADNe - only importation
  if(c("Sites_ADNe") %in%  names(data)  ){

    cat("\nImporting", crayon::cyan("Sites_ADNe"), "table KEY:\n")

    Numero_unique_site   <- data$Sites_ADNe$Numero_unique_site_ADNe

    dup <- Numero_unique_site[duplicated(Numero_unique_site)]

    cat("\n", length(Numero_unique_site), "Numero_unique_site detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

  } else{
    cat(crayon::red("\nNo Sites_ADNe table detected\n"))
    Numero_unique_site   <- NULL
  }

  # Station : 1 check

  if(c("Stations_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Stations_ADNe"), "table KEY:\n")

    Numero_unique_station   <- data$Stations_ADNe$Numero_unique_station_ADNe

    dup <- Numero_unique_station[duplicated(Numero_unique_station)]

    cat("\n", length(Numero_unique_station), "Numero_unique_station detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with site

    if(!is.null(Numero_unique_site)){

      if(all(data$Stations_ADNe$Numero_unique_site_ADNe %in% Numero_unique_site)){
        cat(crayon::green("\nAll the Numero_unique_site_ADNe observed exist in the Sites_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_site_ADNe observed exist in the Sites_ADNe table.\n",


                        paste(unique(data$Stations_ADNe$Numero_unique_site_ADNe[!data$Stations_ADNe$Numero_unique_site_ADNe %in% Numero_unique_site]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_site_ADNe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo Specimens table detected\n"))
    Numero_unique_station   <- NULL
  }


  # ECHANTILLON : 2 checks

  if(c("Echantillons_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Echantillons_ADNe"), "table KEY:\n")

    Numero_unique_echantillon <- data$Echantillons_ADNe$Numero_unique_echantillon_ADNe

    dup <- Numero_unique_echantillon[duplicated(Numero_unique_echantillon)]

    cat("\n", length(Numero_unique_echantillon), "Numero_unique_echantillon_ADNe detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with sites

    if(!is.null(Numero_unique_site)){

      if(all(data$Echantillons_ADNe$Numero_unique_site_ADNe %in% Numero_unique_site)){
        cat(crayon::green("\nAll the Numero_unique_site_ADNe observed exist in the Sites_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_site_ADNe observed exist in the Sites_ADNe table.\n",


                        paste(unique(data$Echantillon_ADNe$Numero_unique_site_ADNe[!data$Echantillon_ADNe$Numero_unique_site_ADNe %in% Numero_unique_site]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_site_ADNe could not be checked in this table\n"))

    }

    # Check with stations

    if(!is.null(Numero_unique_station)){

      if(all(data$Echantillons_ADNe$Numero_unique_station_ADNe %in% Numero_unique_station)){
        cat(crayon::green("\nAll the Numero_unique_station_ADNe observed exist in the Stations_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_station_ADNe observed exist in the Stations_ADNe table.\n",


                        paste(unique(data$Echantillon_ADNe$Numero_unique_station_ADNe[!data$Echantillon_ADNe$Numero_unique_station_ADNe %in% Numero_unique_station]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_station_ADNe could not be checked in this table\n"))

    }


  } else{
    cat(crayon::red("\nNo Echantillons_ADNe table detected\n"))
    Numero_unique_echantillon   <- NULL
  }

  # FILTRE : 1 check

  if(c("Filtres_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Filtres_ADNe"), "table KEY:\n")

    Numero_unique_filtre <- data$Filtres_ADNe$Numero_unique_filtre_ADNe

    dup <- Numero_unique_filtre[duplicated(Numero_unique_filtre)]

    cat("\n", length(Numero_unique_filtre), "Numero_unique_filtre_ADNe detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with Echantillons

    if(!is.null(Numero_unique_echantillon)){

      if(all(data$Filtres_ADNe$Numero_unique_echantillon_ADNe %in% Numero_unique_echantillon)){
        cat(crayon::green("\nAll the Numero_unique_echantillon_ADNe observed exist in the Echantillon_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_echantillon_ADNe observed exist in the Site_ADNe table.\n",


                        paste(unique(data$Filtres_ADNe$Numero_unique_echantillon_ADNe[!data$Filtres_ADNe$Numero_unique_echantillon_ADNe %in% Numero_unique_echantillon_ADNe]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_echantillon_ADNe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo Filtres_ADNe table detected\n"))
    Numero_unique_filtre   <- NULL
  }


  # EXTRAIT : 1 checks

  if(c("Extraits_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Extraits_ADNe"), "table KEY:\n")

    Numero_unique_extrait <- data$Extraits_ADNe$Numero_unique_extrait_ADNe

    dup <- Numero_unique_extrait[duplicated(Numero_unique_extrait)]

    cat("\n", length(Numero_unique_extrait), "Numero_unique_extrait_ADNe detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

    # Check with filtre

    if(!is.null(Numero_unique_filtre)){

      if(all(data$Extraits_ADNe$Numero_unique_filtre_ADNe %in% Numero_unique_filtre)){
        cat(crayon::green("\nAll the Numero_unique_filtre_ADNe observed exist in the Filtres_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_filtre_ADNe observed exist in the Filtre_ADNe table.\n",


                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_filtre_ADNe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo Extraits_ADNe table detected\n"))
    Numero_unique_extrai   <- NULL
  }



  # COURBE - only importation
  if(c("Courbe_etalonnage_ADNe") %in%  names(data)  ){

    cat("\nImporting", crayon::cyan("Courbe_etalonnage_ADNe"), "table KEY:\n")

   Courbe_ID   <- data$Courbe_etalonnage_ADNe$Courbe_ID

    dup <- Courbe_ID[duplicated(Courbe_ID)]

    cat("\n", length(Courbe_ID), "Courbe_ID detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

  } else{
    cat(crayon::red("\nNo Courbe_etalonnage_ADNe table detected\n"))
    Courbe_ID  <- NULL
  }




  # QPCR inhibition : 1 checks

  if(c("qPCR_inhibition_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("qPCR_inhibition_ADNe"), "table KEY:\n")

    # Check with Extrait

    if(!is.null(Numero_unique_extrait)){

      if(all(data$qPCR_inhibition_ADNe$Numero_unique_extrait_ADNe %in% Numero_unique_extrait)){
        cat(crayon::green("\nAll the Numero_unique_extrait_ADNe observed exist in the Extraits_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait_ADNe observed exist in the Extraits_ADNe table.\n",


                        paste(unique(data$qPCR_inhibition_ADNe$Numero_unique_extrait_ADNe[!data$qPCR_inhibition_ADNe$Numero_unique_extrait_ADNe %in% Numero_unique_extrait]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extrait_ADNe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo qPCR_inhibition_ADNe table detected\n"))

  }


  # QPCR : 2 checks

  if(c("qPCR_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("qPCR_ADNe"), "table KEY:\n")



    qpcr_ID   <- data$qPCR_ADNe$ID_qPCR_ADNe

    dup <- qpcr_ID[duplicated(qpcr_ID)]

    cat("\n", length(qpcr_ID), "ID_qPCR_ADNe detected\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }


    # Check with Extrait

    if(!is.null(Numero_unique_extrait)){

      if(all(data$qPCR_ADNe$Numero_unique_extrait_ADNe %in% Numero_unique_extrait)){
        cat(crayon::green("\nAll the Numero_unique_extrait_ADNe observed exist in the Extraits_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait_ADNe observed exist in the Extraits_ADNe table.\n",


                        paste(unique(data$qPCR_ADNe$Numero_unique_extrait_ADNe[!data$qPCR_ADNe$Numero_unique_extrait_ADNe %in% Numero_unique_extrait]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extrait_ADNe could not be checked in this table\n"))

    }

    # Check with Courbe

    if(!is.null(Courbe_ID)){

      if(all(data$qPCR_ADNe$Courbe_ID %in% Courbe_ID)){
        cat(crayon::green("\nAll the Courbe_ID observed exist in the Courbe_etalonnage_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Courbe_ID observed exist in the Courbe_etalonnage_ADNe table.\n",


                        paste(unique(data$qPCR_ADNe$Courbe_ID[!data$qPCR_ADNe$Courbe_ID %in% Courbe_ID]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nCourbe_ID could not be checked in this table\n"))
      }

  } else{
    cat(crayon::red("\nNo qPCR_ADNe table detected\n"))

    qpcr_ID  <- NULL

  }

  # qpc_qnc : 1 checks

  if(c("QNC_QPC_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("QNC_QPC_ADNe"), "table KEY:\n")

    # Check with qPCR

    if(!is.null(qpcr_ID)){

      if(all(data$QNC_QPC_ADNe$ID_qPCR_ADNe %in%  qpcr_ID)){
        cat(crayon::green("\nAll the ID_qPCR_ADNe observed exist in the qPCR_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the ID_qPCR_ADNe observed exist in the qPCR_ADNe table.\n",


                        paste(unique(data$QNC_QPC_ADNe$ID_qPCR_ADNe[!data$QNC_QPC_ADNe$ID_qPCR_ADNe %in% qpcr_ID]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nID_qPCR_ADNe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo QNC_QPC_ADNe table detected\n"))

  }





  # Sequencage : 1 checks

  if(c("Sequencage_Sanger_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Sequencage_Sanger_ADNe"), "table KEY:\n")

    # Check with qPCR

    if(!is.null(qpcr_ID)){

      if(all(data$Sequencage_Sanger_ADNe$ID_qPCR_ADNe %in%  qpcr_ID)){
        cat(crayon::green("\nAll the ID_qPCR_ADNe observed exist in the qPCR_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the ID_qPCR_ADNe observed exist in the qPCR_ADNe table.\n",


                        paste(unique(data$Sequencage_Sanger_ADNe$ID_qPCR_ADNe[!data$Sequencage_Sanger_ADNe$ID_qPCR_ADNe %in% qpcr_ID]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nID_qPCR_ADNe could not be checked in this table\n"))

    }

  } else{
    cat(crayon::red("\nNo Sequencage_Sanger_ADNe table detected\n"))

  }




  # Purification - only importation
  if(c("Purification_librairies_ADNe") %in%  names(data)  ){

    cat("\nImporting", crayon::cyan("Purification_librairies_ADNe"), "table KEY:\n")

    Purif_ID   <- data$Purification_librairies_ADNe$Numero_unique_librairie_ADNe

    dup <- Purif_ID[duplicated(Purif_ID)]

    cat("\n", length(Purif_ID), "Numero_unique_librairie_ADNe\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

  } else{
    cat(crayon::red("\nNo Purification_librairies_ADNe table detected\n"))
    Purif_ID  <- NULL
  }



  # Externe - only importation
  if(c("Analyses_externes_librairies_ADNe") %in%  names(data)  ){

    cat("\nImporting", crayon::cyan("Analyses_externes_librairies_ADNe"), "table KEY:\n")

    Ext_ID   <- data$Analyses_externes_librairies_ADNe$Numero_unique_librairie_SeqReady_ADNe

    dup <- Ext_ID[duplicated(Ext_ID)]

    cat("\n", length(Ext_ID), "Numero_unique_librairie_SeqReady_ADNe\n")

    if(length(dup) > 0){
      cat(crayon::red("\nDuplicated keys were observed:", paste(dup, collapse = ", "),
                      "\nThis will be problematic with the importation into ACCESS...\n"))
    }

  } else{
    cat(crayon::red("\nNo Analyses_externes_librairies_ADNe table detected\n"))
    Ext_ID  <- NULL
  }



  # Librairie : 3 checks

  if(c("Librairies_ADNe") %in%  names(data)  ){
    cat("\nImporting", crayon::cyan("Librairies_ADNe"), "table KEY:\n")

    # Check with Numero_unique_extrait_ADNe

    if(!is.null(Numero_unique_extrait)){

      if(all(data$Librairies_ADNe$Numero_unique_extrait_ADNe %in%  Numero_unique_extrait)){
        cat(crayon::green("\nAll the Numero_unique_extrait observed exist in the Extrait_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_extrait observed exist in the Extraits_ADNe table.\n",


                        paste(unique(data$Librairies_ADNe$Numero_unique_extrait_ADNe[!data$Librairies_ADNe$Numero_unique_extrait_ADNe %in% Numero_unique_extrait]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_extrait could not be checked in this table\n"))

    }



    # Check with Numero_unique_extrait_ADNe

    if(!is.null(Purif_ID)){

      if(all(data$Librairies_ADNe$Numero_unique_librairie_ADNe %in%  Purif_ID)){
        cat(crayon::green("\nAll the Numero_unique_librairie_ADNe observed exist in the Purification_librairies_ADNe table.\n"))

      } else {

        cat(crayon::red("\nNot all the Numero_unique_librairie_ADNe observed exist in the Purification_librairies_ADNe table.\n",


                        paste(unique(data$Librairies_ADNe$Numero_unique_librairie_ADNe[!data$Librairies_ADNe$Numero_unique_librairie_ADNe %in% Purif_ID]), collapse = ", ")
                        , "\nare missing\n"))
      }

    } else {
      cat(crayon::red("\nNumero_unique_librairie_ADNe could not be checked in this table\n"))

    }


  } else{
    cat(crayon::red("\nNo librairies_ADNe table detected\n"))

  }

  # Check with Numero_unique_librairie_SeqReady_ADNe

  if(!is.null(Ext_ID)){

    if(all(data$Librairies_ADNe$Numero_unique_librairie_SeqReady_ADNe %in%  Ext_ID)){
      cat(crayon::green("\nAll the Numero_unique_librairie_SeqReady_ADNe observed exist in the Analyses_externes_librairies_ADNe table.\n"))

    } else {

      cat(crayon::red("\nNot all the Numero_unique_librairie_SeqReady_ADNe observed exist in the Analyses_externes_librairies_ADNe table.\n",


                      paste(unique(data$Librairies_ADNe$Numero_unique_librairie_SeqReady_ADNe[!data$Librairies_ADNe$Numero_unique_librairie_SeqReady_ADNe %in% Ext_ID]), collapse = ", ")
                      , "\nare missing\n"))
    }

  } else {
    cat(crayon::red("\nNumero_unique_librairie_SeqReady_ADNe could not be checked in this table\n"))

  }



  cat(crayon::green("\nVerification done!", emojifont::emoji("ocean"), "\n"))



} # END of the function





