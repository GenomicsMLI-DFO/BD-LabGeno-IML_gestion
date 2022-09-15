#' @title Upload Excel template
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
#' @param group Name of the sheet in the Excel file containing group data
#' @param tissu Name of the sheet in the Excel file containing tissue data
#' @param extrait Name of the sheet in the Excel file containing the DNA/RNA extract data
#' @param analyse_ext Name of the sheet in the Excel file containing external analysis info
#' @param sexage of the sheet in the Excel file containing the sex determining method
#' @param dloop Name of the sheet in the Excel file containing the dloop info
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso List relevant other functions [littleforecast()].
#'
#` @references
#' List references
#' @export
upload_gabarit_ADN <- function(path,
                               skip=1,
                               specimen = "Specimen",
                               groupe = "Groupe",
                               tissu = "Tissu",
                               extraitADN = "ExtraitADN_ARN",
                               analyse_ext = "Analyse_Externe",
                               sexage = "Sexage",
                               dloop = "DLoop"
                               ){
  # Etape 1 - verifier que les noms suivent le gabarit

  `%nin%` = Negate(`%in%`)

  sheet.observed <- readxl::excel_sheets(path = path)

  cat("\nThe sheets detected are",  paste(sheet.observed, collapse = ", "), "\n")

  sheet.to.load <- c(specimen, groupe, tissu, extraitADN, analyse_ext, sexage, dloop)

  if( !all(sheet.to.load %in% sheet.observed)){
         stop(paste("\nThe sheet to be uploaded (",  paste(sheet.to.load, collapse = ", "),") doesn't fit the one detected. You can modified their names as an argument within this function or directly within the Excel file."))
  }

  # Etape 2 - loader chacune des pages, et les mettre dans une liste
  # On pourrait imprimer certaines statistiques

  excel.ls <- list()

  # Load specimen

  if(!is.null(specimen)){

    cat("\nLoading SPECIMEN\n")

     temp.df <-  readxl::read_excel(path = path, sheet = specimen, skip = skip,col_types = "text", .name_repair = "minimal")

     cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

       excel.ls[["Specimen"]] <- temp.df

    }

  }

  # Load groupe

  if(!is.null(groupe)){

    cat("\nLoading GROUPE\n")

    temp.df <-  readxl::read_excel(path = path, sheet = groupe, skip = skip,col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Groupe"]] <- temp.df

    }

  }

  # Load tissu

  if(!is.null(tissu)){

    cat("\nLoading TISSU\n")

    temp.df <-  readxl::read_excel(path = path, sheet = tissu, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){


      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }


      excel.ls[["Tissu"]] <- temp.df

    }

  }

  # Load extrait

  if(!is.null(extraitADN)){

    cat("\nLoading EXTRAIT_ADN\n")

    temp.df <-  readxl::read_excel(path = path, sheet = extraitADN, skip = skip, col_types = "text",.name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["ExtraitADN_ARN"]] <- temp.df

    }

  }

  # Load Analyse

  if(!is.null(analyse_ext)){

    cat("\nLoading ANALYSE_EXT\n")

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

    cat("\nLoading SEXAGE\n")

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


  # Load dloop

  if(!is.null(dloop)){

    cat("\nLoading DLOOP\n")

    temp.df <-  readxl::read_excel(path = path, sheet = dloop, skip = skip, col_types = "text", .name_repair = "minimal")

    cat("A dataframe of", ncol(temp.df), "columns and", nrow(temp.df), "rows was uploaded\n")

    if(nrow(temp.df)>0){
      dup.prob <- names(temp.df)[duplicated(names(temp.df))]
      if(length(dup.prob > 0)){

        cat(crayon::red("WARNING: The column(s)", paste(dup.prob, collapse = ", "), "appeared more than one time, you should check this prior to the importation in R ...\n"))

      }

      excel.ls[["Dloop"]] <- temp.df

    }

  }



  # Etape 3 - retourner la liste - c'est à partir d'elle qu'on va travailler

  cat(crayon::green("The sheets", paste(names(excel.ls), collapse = ", "), "have been uploaded", emojifont::emoji("unicorn")  ,"\n\n"))

  return(excel.ls)

}
