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
#' @param sequencage Name of the sheet in the Excel file containing the sequencage info
#' @param hormone Name of the sheet in the Excel file containing the hormone info

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
                               hormone = "Hormones"
                               ){
  # Etape 1 - verifier que les noms suivent le gabarit

  `%nin%` = Negate(`%in%`)

  sheet.observed <- readxl::excel_sheets(path = path)

  cat("\nThe sheets detected are",  paste(sheet.observed, collapse = ", "), "\n")

  sheet.to.load <- c(specimen, groupe, tissu, extraitADN, analyse_ext, sexage, sequencage, hormone)

  if( !all(sheet.to.load %in% sheet.observed)){
         stop(paste("\nThe sheet to be uploaded (",  paste(sheet.to.load, collapse = ", "),") doesn't fit the one detected. You can modified their names as an argument within this function or directly within the Excel file. NULL can be added as an argument if a sheet is totally absent."))
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
                                     table.access = c("Groupes", "Specimens", "Tissus", "Extraits_ADN_ARN", "Analyse_Externe", "Sexage", "Sequencage", "Hormones")
                                     ){

excel.files <- list.files(path, pattern = "xlsx") %>% stringr::str_subset("\\$", negate = T)

cat("\n", "Looking for Excel spreadsheets in",  path, "\n",length(excel.files), "files are detected:", paste(excel.files, collapse = ", "), "\n")

combine.ls <- list(Groupes = data.frame(),
                   Specimens = data.frame(),
                   Tissus = data.frame(),
                   Extraits_ADN_ARN = data.frame(),
                   Analyse_Externe = data.frame(),
                   Sexage = data.frame(),
                   Sequencage = data.frame()
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


