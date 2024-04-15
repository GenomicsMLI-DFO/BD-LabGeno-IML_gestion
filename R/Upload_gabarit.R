#' @title Upload Excel DNA template
#'
#' @description
#' Function to upload an Excel file based on the predefined template
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param path Path to the excel spreadsheet
#' @param skip N row to skip. Default value is 1 as there's a warning in the original file
#' @param specimen Name of the sheet in the Excel file containing specimen data
#' @param groupe Name of the sheet in the Excel file containing group data
#' @param tissu Name of the sheet in the Excel file containing tissue data
#' @param extraitADN Name of the sheet in the Excel file containing the DNA/RNA extract data
#' @param analyse_ext Name of the sheet in the Excel file containing external analysis info
#' @param sexage of the sheet in the Excel file containing the sex determining method
#' @param sequencage Name of the sheet in the Excel file containing the sequencage info
#' @param hormone Name of the sheet in the Excel file containing the hormone info
#' @param wgs Name of the sheet in the Excel file containing the WGS info
#' @param wgs_pool Name of the sheet in the Excel file containing the WGS pool info
#' @param analyse_ext_lib Name of the sheet in the Excel file containing the WGS pool info
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso
#'
#` @references
#' List references
#' @export
upload_gabarit_ADN <- function(path,
                               skip=0,
                               specimen = "Specimens",
                               groupe = "Groupes",
                               tissu = "Tissus",
                               extraitADN = "Extraits_ADN_ARN",
                               analyse_ext = "Analyse_Externe",
                               sexage = "Sexage",
                               sequencage = "Sequencage",
                               hormone = "Hormones",
                               wgs = "WGS",
                               wgs_pool = "WGS_Pool",
                               analyse_ext_lib = "Externe_librairies_WGS"
                               ){
  # Etape 1 - verifier que les noms suivent le gabarit

  `%nin%` = Negate(`%in%`)

  sheet.observed <- readxl::excel_sheets(path = path)

  cat("\nThe sheets detected are",  paste(sheet.observed, collapse = ", "), "\n")

  sheet.to.load <- c(specimen, groupe, tissu, extraitADN, analyse_ext, sexage, sequencage, hormone)

  if( !all(sheet.to.load %in% sheet.observed)){
    stop(paste("\nThe sheet to be uploaded doesn't fit the one detected.\n\nThese ones are problematics:", paste(setdiff(sheet.to.load, sheet.observed), collapse = ", "),
               "\n\nYou can modified their names as an argument within this function or directly within the Excel file. NULL can be added as an argument if a sheet is totally absent."))
  }

  # Etape 2 - loader chacune des pages, et les mettre dans une liste
  # On pourrait imprimer certaines statistiques

  excel.ls <- list()

  # Load groupe

  if(!is.null(groupe)){

    cat("\nLoading", crayon::cyan("Groupes"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = groupe, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Groupes"]] <- temp.df

    }

  }

  # Load specimen

  if(!is.null(specimen)){

    cat("\nLoading", crayon::cyan("Specimens"),"\n")

     temp.df <-  readxl::read_excel(path = path, sheet = specimen, skip = skip,col_types = "text", .name_repair = "minimal")

     cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

       excel.ls[["Specimens"]] <- temp.df

    }

  }


  # Load tissu

  if(!is.null(tissu)){

    cat("\nLoading", crayon::cyan("Tissus"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = tissu, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){


      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }


      excel.ls[["Tissus"]] <- temp.df

    }

  }

  # Load extrait

  if(!is.null(extraitADN)){

    cat("\nLoading", crayon::cyan("Extraits_ADN_ARN"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = extraitADN, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Extraits_ADN_ARN"]] <- temp.df

    }

  }

  # Load Analyse

  if(!is.null(analyse_ext)){

    cat("\nLoading", crayon::cyan("Analyse_Externe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = analyse_ext, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Analyse_Externe"]] <- temp.df

    }

  }


  # Load sexage

  if(!is.null(sexage)){

    cat("\nLoading", crayon::cyan("Sexage"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = sexage, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Sexage"]] <- temp.df

    }

  }


  # Load sequencage

  if(!is.null(sequencage)){

    cat("\nLoading", crayon::cyan("Sequencage"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = sequencage, skip = skip, col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Sequencage"]] <- temp.df

    }

  }

  # Load sexage

  if(!is.null(hormone)){

    cat("\nLoading", crayon::cyan("Hormones"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = hormone, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Hormones"]] <- temp.df

    }

  }

  # Load WGS

  if(!is.null(wgs)){

    cat("\nLoading", crayon::cyan("WGS"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = wgs, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["WGS"]] <- temp.df

    }

  }

  # Load WGS_pool

  if(!is.null(wgs_pool)){

    cat("\nLoading", crayon::cyan("WGS_Pool"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = wgs_pool, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["WGS_Pool"]] <- temp.df

    }

  }

  # Load Analyses_externes_librairies_WGS

  if(!is.null(analyse_ext_lib)){

    cat("\nLoading", crayon::cyan("Externe_librairies_WGS"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = analyse_ext_lib, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Externe_librairies_WGS"]] <- temp.df

    }

  }

  # Etape 3 - retourner la liste - c'est à partir d'elle qu'on va travailler

  cat(crayon::green("\nThe sheets", paste(names(excel.ls), collapse = ", "), "have been uploaded", emojifont::emoji("unicorn")  ,"\n\n"))

  return(excel.ls)

}


#' @title Upload and combine multiple corrected tables
#'
#' @description
#' Function to upload and combined corrected Excel files
#'
#'
#' @param path Path to the folder were the excel files are
#' @param table.access List of sheets that will be read (only if they exist)
#'
#' @examples
#' # provide some examples of how to use your function
#'
#' @seealso [upload_gabarit_ADN()].
#'
#` @references
#' List references
#' @export

combine_multiple_gabarit <- function(path,
                                     table.access = c("Groupes", "Specimens", "Tissus", "Extraits_ADN_ARN", "Analyse_Externe", "Sexage", "Sequencage", "Hormones", "WGS", "WGS_Pool", "Analyses_externes_librairies_WGS",
                                                      "Sites_ADNe", "Stations_ADNe", "Echantillons_ADNe", "Filtres_ADNe", "Extraits_ADNe", "qPCR_ADNe", "qPCR_inhibition_ADNe",
                                                      "QNC_QPC_ADNe", "Librairies_ADNe", "Purification_librairies_ADNe", "Analyses_externes_librairies_ADNe", "Courbe_etalonnage_ADNe", "Sequencage_Sanger_ADNe")
                                     ){

excel.files <- list.files(path, pattern = "xlsx") %>% stringr::str_subset("\\$", negate = T)

cat("\n", "Looking for Excel spreadsheets in",  path, "\n",length(excel.files), "files are detected:", paste(excel.files, collapse = ", "), "\n")

combine.ls <- list(Groupes = data.frame(),
                   Specimens = data.frame(),
                   Tissus = data.frame(),
                   Extraits_ADN_ARN = data.frame(),
                   Analyse_Externe = data.frame(),
                   Sexage = data.frame(),
                   Sequencage = data.frame(),
                   Sites_ADNe = data.frame(),
                   Stations_ADNe = data.frame(),
                   Echantillons_ADNe = data.frame(),
                   Filtres_ADNe = data.frame(),
                   Extraits_ADNe = data.frame(),
                   qPCR_ADNe = data.frame(),
                   qPCR_inhibition_ADNe = data.frame(),
                   QNC_QPC_ADNe = data.frame(),
                   Sequencage_Sanger_ADNe = data.frame(),
                   Librairies_ADNe = data.frame(),
                   Purification_librairies_ADNe = data.frame(),
                   Analyses_externes_librairies_AD =data.frame(),
                   Courbe_etalonnage_ADNe = data.frame()


                   )

n.test <- 0

for(x in excel.files){

  table.names <- readxl::excel_sheets(file.path(path, x))

  int.ls <- list()

  cat("\nLooking for", crayon::cyan(x), "\n")

  for(i in table.names){

  if(i %in% table.access ){

  cat("\n", crayon::cyan(i), "was detected and will be uploaded: ")


  df.int <- readxl::read_excel(file.path(path, x), sheet = i, col_types = "text", .name_repair = "minimal")

  cat("", ncol(df.int), "columns and", nrow(df.int), "rows were uploaded\n")

  int.ls[[i]] <-  df.int

  combine.ls[[i]]  <- dplyr::bind_rows(combine.ls[[i]] , df.int )

  n.test <- n.test + 1
    } #

  } #END of loop over table name


}

cat(crayon::green("\n",n.test, "tables have been combined", emojifont::emoji("bomb")  ,"\n\n"))

return(combine.ls)


}

#' @title Upload and combine multiple corrected tables
#'
#' @description
#' Function to upload and combined corrected Excel files
#'
#'
#' @param path Path to the folder were the excel files are
#' @param table.access List of sheets that will be read (only if they exist)
#'
#' @examples
#' # provide some examples of how to use your function
#'
#' @seealso [upload_gabarit_ADN()].
#'
#` @references
#' List references
#' @export

combine_multiple_gabarit_ADNe <- function(path,
                                          table.access = c(
                                                      "Sites_ADNe", "Stations_ADNe", "Echantillons_ADNe", "Filtres_ADNe", "Extraits_ADNe", "qPCR_ADNe", "qPCR_inhibition_ADNe",
                                                      "QNC_QPC_ADNe", "Librairies_ADNe", "Purification_librairies_ADNe", "Analyses_externes_librairies_AD", "Librairies_SeqReady_ADNe", "Courbe_etalonnage_ADNe", "Sequencage_Sanger_ADNe")
){

  excel.files <- list.files(path, pattern = "xlsx") %>% stringr::str_subset("\\$", negate = T)

  cat("\n", "Looking for Excel spreadsheets in",  path, "\n",length(excel.files), "files are detected:", paste(excel.files, collapse = ", "), "\n")

  combine.ls <- list(
                     Sites_ADNe = data.frame(),
                     Stations_ADNe = data.frame(),
                     Echantillons_ADNe = data.frame(),
                     Filtres_ADNe = data.frame(),
                     Extraits_ADNe = data.frame(),
                     qPCR_ADNe = data.frame(),
                     qPCR_inhibition_ADNe = data.frame(),
                     QNC_QPC_ADNe = data.frame(),
                     Sequencage_Sanger_ADNe = data.frame(),
                     Librairies_ADNe = data.frame(),
                     Purification_librairies_ADNe = data.frame(),
                     Analyses_externes_librairies_AD = data.frame(),
                     Librairies_SeqReady_ADNe = data.frame(),
                     Courbe_etalonnage_ADNe = data.frame()
  )

  n.test <- 0

  for(x in excel.files){

    table.names <- readxl::excel_sheets(file.path(path, x))

    int.ls <- list()

    cat("\nLooking for", crayon::cyan(x), "\n")

    for(i in table.names){

      if(i %in% table.access ){

        cat("\n", crayon::cyan(i), "was detected and will be uploaded: ")


        df.int <- readxl::read_excel(file.path(path, x), sheet = i, col_types = "text", .name_repair = "minimal")

        cat("", ncol(df.int), "columns and", nrow(df.int), "rows were uploaded\n")

        int.ls[[i]] <-  df.int

        combine.ls[[i]]  <- dplyr::bind_rows(combine.ls[[i]] , df.int )

        n.test <- n.test + 1
      } #

    } #END of loop over table name


  }

  cat(crayon::green("\n",n.test, "tables have been combined", emojifont::emoji("bomb")  ,"\n\n"))

  return(combine.ls)


}


#' @title Upload Excel eDNA template
#'
#' @description
#' Function to upload an Excel file based on the predefined eDNA template
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param path Path to the excel spreadsheet
#' @param skip N row to skip. Default value is 0.
#' @param site Name of the sheet in the Excel file containing site data
#' @param station Name of the sheet in the Excel file containing station data
#' @param echantillon Name of the sheet in the Excel file containing sample data
#' @param filtre Name of the sheet in the Excel file containing the filter data
#' @param extrait Name of the sheet in the Excel file containing extract data
#' @param qpcr_inhibition of the sheet in the Excel file containing the qpcr inihibiton info
#' @param qpcr Name of the sheet in the Excel file containing the qpcr results
#' @param qnc_qpc Name of the sheet in the Excel file containing the qpcr control info
#' @param sequencage Name of the sheet in the Excel file containing the Sanger sequencing info
#' @param courbe Name of the sheet in the Excel file containing the standard curve info
#' @param librairies Name of the sheet in the Excel file containing the librairy info
#' @param purification Name of the sheet in the Excel file containing the librairy purification info
#' @param analyses_ext_lib Name of the sheet in the Excel file containing the librairy sequencing info
#' @param librairies_seq Name of the sheet in the Excel file containing the Librairie SeqReady ADNe info
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso
#'
#` @references
#' List references
#' @export

upload_gabarit_ADNe <- function(path,
                                skip=0,
                                site = "Sites_ADNe",
                                station = "Stations_ADNe",
                                echantillon = "Echantillons_ADNe",
                                filtre = "Filtres_ADNe",
                                extrait = "Extraits_ADNe",
                                qpcr_inhibition = "qPCR_inhibition_ADNe",
                                qpcr = "qPCR_ADNe",
                                qnc_qpc = "QNC_QPC_ADNe",
                                sequencage = "Sequencage_Sanger_ADNe",
                                courbe = "Courbe_etalonnage_ADNe",
                                librairies = "Librairies_ADNe",
                                purification = "Purification_librairies_ADNe",
                                analyses_ext_lib = "Analyses_externes_librairies_ADNe",
                                librairies_seq = "Librairies_SeqReady_ADNe"
){
  # Etape 1 - verifier que les noms suivent le gabarit

  `%nin%` = Negate(`%in%`)

  sheet.observed <- readxl::excel_sheets(path = path)

  cat("\nThe sheets detected are",  paste(sheet.observed, collapse = ", "), "\n")

  sheet.to.load <- c(site, station, echantillon, filtre, extrait, qpcr_inhibition, qpcr, sequencage, courbe, librairies, purification, analyses_ext_lib, librairies_seq)

  if( !all(sheet.to.load %in% sheet.observed)){
    stop(paste("\nThe sheet to be uploaded doesn't fit the one detected.\n\nThese ones are problematics:", paste(setdiff(sheet.to.load, sheet.observed), collapse = ", "),
               "\n\nYou can modified their names as an argument within this function or directly within the Excel file. NULL can be added as an argument if a sheet is totally absent."))
  }

  # Etape 2 - loader chacune des pages, et les mettre dans une liste
  # On pourrait imprimer certaines statistiques

  excel.ls <- list()

  # Load sites

  if(!is.null(site)){

    cat("\nLoading", crayon::cyan("Sites_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = site, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Sites_ADNe"]] <- temp.df

    }

  }

  # Load station

  if(!is.null(station)){

    cat("\nLoading", crayon::cyan("Stations_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = station, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Stations_ADNe"]] <- temp.df

    }

  }


  # Load echantillon

  if(!is.null(echantillon)){

    cat("\nLoading", crayon::cyan("Echantillons_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = echantillon, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Echantillons_ADNe"]] <- temp.df

    }

  }

  # Load filtre

  if(!is.null(filtre)){

    cat("\nLoading", crayon::cyan("Filtres_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = filtre, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Filtres_ADNe"]] <- temp.df

    }

  }

  # Load extrait

  if(!is.null(extrait)){

    cat("\nLoading", crayon::cyan("Extraits_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = extrait, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Extraits_ADNe"]] <- temp.df

    }

    }
    # Load qpcr_inhibition

    if(!is.null(qpcr_inhibition)){

      cat("\nLoading", crayon::cyan("qPCR_inhibition_ADNe"),"\n")

      temp.df <-  readxl::read_excel(path = path, sheet = qpcr_inhibition, skip = skip,col_types = "text", .name_repair = "minimal")

      cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

      if(nrow(temp.df)>0){
        dup.prob <- names(temp.df)[duplicated(names(temp.df))]
        if(length(dup.prob > 0)){

          cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

        }

        excel.ls[["qPCR_inhibition_ADNe"]] <- temp.df

      }

      }

      # Load qpcr

      if(!is.null(qpcr)){

        cat("\nLoading", crayon::cyan("qPCR_ADNe"),"\n")

        temp.df <-  readxl::read_excel(path = path, sheet = qpcr, skip = skip,col_types = "text", .name_repair = "minimal")

        cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

        if(nrow(temp.df)>0){
          dup.prob <- names(temp.df)[duplicated(names(temp.df))]
          if(length(dup.prob > 0)){

            cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

          }

          excel.ls[["qPCR_ADNe"]] <- temp.df

        }

        }

  # Load qpcr

  if(!is.null(qnc_qpc)){

    cat("\nLoading", crayon::cyan("QNC_QPC_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = qnc_qpc, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["QNC_QPC_ADNe"]] <- temp.df

    }

  }

  # Load sequencage

  if(!is.null(sequencage)){

    cat("\nLoading", crayon::cyan("Sequencage_Sanger_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = sequencage, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Sequencage_Sanger_ADNe"]] <- temp.df

    }

  }


  # Load courbe

  if(!is.null(courbe)){

    cat("\nLoading", crayon::cyan("Courbe_etalonnage_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = courbe, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Courbe_etalonnage_ADNe"]] <- temp.df

    }

  }

  # Load librairies

  if(!is.null(librairies)){

    cat("\nLoading", crayon::cyan("Librairie_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = librairies, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Librairies_ADNe"]] <- temp.df

    }

  }

  # Load purification

  if(!is.null(purification)){

    cat("\nLoading", crayon::cyan("Purification_librairies_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = purification, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Purification_librairies_ADNe"]] <- temp.df

    }

  }

  # Load Analyses_externes_librairies_ADNe

  if(!is.null(analyses_ext_lib)){

    cat("\nLoading", crayon::cyan("Analyses_externes_librairies_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = analyses_ext_lib, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Analyses_externes_librairies_ADNe"]] <- temp.df

    }

  }

  # Load Analyses_externes_librairies_ADNe

  if(!is.null(librairies_seq)){

    cat("\nLoading", crayon::cyan("Librairies_SeqReady_ADNe"),"\n")

    temp.df <-  readxl::read_excel(path = path, sheet = librairies_seq, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Librairies_SeqReady_ADNe"]] <- temp.df

    }

  }




  # Etape 3 - retourner la liste - c'est à partir d'elle qu'on va travailler

  cat(crayon::green("\nThe sheets", paste(names(excel.ls), collapse = ", "), "have been uploaded", emojifont::emoji("unicorn")  ,"\n\n"))

  return(excel.ls)

}


