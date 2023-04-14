#' @title Check relations between tables
#'
#' @description
#' Function to check that column name and order follow the predefined template
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
