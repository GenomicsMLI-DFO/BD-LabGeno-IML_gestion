#' @title Check the Projet column specifically
#'
#' @description
#' Function to check that column name and order follow the predefined template
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data List containing important tables.
#' @param DB Name of the DB in the ODBC.
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_project  <- function(data, DB = "LabGeno"){

  col.int <-   "Nom_projet"

  model.projet <-   load_DB(DB = DB)

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  Projet <-  model.projet  %>% dplyr::pull(col.int)
  Projet <- Projet[order(Projet)]
  #new.list <- list()
  cat("\nChecking for columns called", crayon::cyan( col.int), "\n")

  for(x in names(data)){

    tab.int <- data[[x]]

    if(col.int %in% names(tab.int)){

      cat("\nA column", col.int, "was observed in the table", crayon::cyan(x), "\n")

      tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_replace_all(" ", "_") %>% as.character()
      tab.int[which( tab.int[, col.int] == "NA") , col.int] <- NA

      # Check pour les NA

      n.NA <- nrow(tab.int[which( is.na(tab.int[, col.int])) , col.int])

      if(n.NA > 0){

        answer <- NULL
        answer <- menu(title = paste("\n", n.NA, "missing values were observed. What should the project be ? (0 to cancel corrections)" ),
                       graphics = F,
                       choice = Projet
                       #choice = c(crayon::red("Missing value"), Projet)
        )

        #  if(answer == 1) { # Change to a real missing value
        #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
        #              cat(j, "was replaced with NA's\n")
        #            }

        if(answer %in% 1:(length(Projet))){

          new.value <- Projet[[answer]]

          tab.int[which( is.na(tab.int[, col.int])) , col.int] <- new.value

          cat("NA was replaced with", new.value, "\n")

        }

        if(answer == 0){
          cat("No replacement done. Please change NA manually as no missing value should remained. \n")

        }

      }

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]


      cat("\n Observed values are", crayon::cyan(paste(observed.vec, collapse = ", ")), "\n")

      if(length(observed.vec) > 0)  {  # only if there are values

        # Loop over unique value observed
        for(j in observed.vec){

          if((j %in% Projet) == F){

            answer <- NULL
            answer <- menu(title = paste("\nWhat value should", crayon::cyan(j), "be ? (0 to cancel corrections)" ),
                           graphics = F,
                           choice = Projet
                           #choice = c(crayon::red("Missing value"), Projet)
            )

            #  if(answer == 1) { # Change to a real missing value
            #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
            #              cat(j, "was replaced with NA's\n")
            #            }

            if(answer %in% 1:(length(Projet))){

              new.value <- Projet[[answer]]

              tab.int[which(tab.int[, col.int] == j) , col.int] <- new.value

              cat(j, "was replaced with", new.value, "\n")

            }

            if(answer == 0){
              cat("No replacement done. Please add this values to the gabarit. \n")

            }


          } # END of loop went something wrong is observed


        }   # END of loop over unique value observed

      } #else { cat(crayon::red("no data within this column\n"))}
      # END of if values observed
      ##
      # Check missing values

      cat( "\n", crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

           nrow(tab.int), "values)\n")

    }# END of loop over column name

    #  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
    #  return(new.list)
    data[[x]] <-  tab.int

  }   # END of the loop over table


  cat(crayon::green("\n\n", col.int , "columns were corrected", emojifont::emoji("cloud")  ,"\n\n"))
  return(data)

} # END of the function


#' @title Check the Primer columns specifically
#'
#' @description
#' Function to check that primers and probes names are in the right format
#'
#' @details
#'
#' @param data List containing important tables.
#' @param DB Name of the DB in the ODBC.
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_primer  <- function(data, DB = "LabGeno"){

  col.F.int <-   "Amorce_F_ID"
  col.R.int <-   "Amorce_R_ID"
  col.P.int <-   "Probe_ID"

  model.F <- load_DB(table = "30a_Amorce_F", DB = DB)
  model.R <- load_DB(table = "30b_Amorce_R", DB = DB)
  model.P <- load_DB(table = "30c_Probe", DB = DB)

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  amorceF <-  model.F  %>% dplyr::pull(col.F.int)
  amorceF <- amorceF[order(amorceF)]

  amorceR <-  model.R  %>% dplyr::pull(col.R.int)
  amorceR <- amorceR[order(amorceR)]

  amorceP <-  model.P  %>% dplyr::pull(col.P.int)
  amorceP <- amorceP[order(amorceP)]

  columns.F <- model.other %>%
    dplyr::filter(Type == "primerF")  %>% pull(Col)

  columns.R <- model.other %>%
    dplyr::filter(Type == "primerR") %>% pull(Col)

  columns.P <- model.other %>%
    dplyr::filter(Type == "primerP") %>% pull(Col)

  #new.list <- list()
  cat("\nChecking for columns called", crayon::cyan( paste(columns.F, sep = ",")), "\n")

  for( col.int in columns.F){

  for(x in names(data)){

    tab.int <- data[[x]]

    if(col.int %in% names(tab.int)){

      cat("\nA column", col.int, "was observed in the table", crayon::cyan(x), "\n")

      tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_replace_all(" ", "_")  %>% stringr::str_replace_all("-", "_") %>%  as.character()
      tab.int[which( tab.int[, col.int] == "NA") , col.int] <- NA

      # Check pour les NA

      n.NA <- nrow(tab.int[which( is.na(tab.int[, col.int])) , col.int])

      if(n.NA > 0){

        answer <- NULL
        answer <- menu(title = paste("\n", n.NA, "missing values were observed. What should the primer be ? (0 to cancel corrections)" ),
                       graphics = F,
                       choice = amorceF
                       #choice = c(crayon::red("Missing value"), Projet)
        )

        #  if(answer == 1) { # Change to a real missing value
        #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
        #              cat(j, "was replaced with NA's\n")
        #            }

        if(answer %in% 1:(length(amorceF))){

          new.value <- Projet[[answer]]

          tab.int[which( is.na(tab.int[, col.int])) , col.int] <- new.value

          cat("NA was replaced with", new.value, "\n")

        }

        if(answer == 0){
          cat("No replacement done. Please change NA manually as no missing value should remained. \n")

        }

      }

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]


      cat("\n Observed values are", crayon::cyan(paste(observed.vec, collapse = ", ")), "\n")

      if(length(observed.vec) > 0)  {  # only if there are values

        # Loop over unique value observed
        for(j in observed.vec){

          if((j %in% amorceF) == F){

            answer <- NULL
            answer <- menu(title = paste("\nWhat value should", crayon::cyan(j), "be ? (0 to cancel corrections)" ),
                           graphics = F,
                           choice = amorceF
                           #choice = c(crayon::red("Missing value"), Projet)
            )

            #  if(answer == 1) { # Change to a real missing value
            #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
            #              cat(j, "was replaced with NA's\n")
            #            }

            if(answer %in% 1:(length(amorceF))){

              new.value <- Projet[[answer]]

              tab.int[which(tab.int[, col.int] == j) , col.int] <- new.value

              cat(j, "was replaced with", new.value, "\n")

            }

            if(answer == 0){
              cat("No replacement done. Please add this values to the primer table prior to the importation. \n")

            }


          } # END of loop went something wrong is observed


        }   # END of loop over unique value observed

      } #else { cat(crayon::red("no data within this column\n"))}
      # END of if values observed
      ##
      # Check missing values

      cat( "\n", crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

           nrow(tab.int), "values)\n")

    }# END of loop over column name

    #  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
    #  return(new.list)
    data[[x]] <-  tab.int

  }   # END of the loop over table



  cat(crayon::green("\n\n", col.int , "columns were corrected", emojifont::emoji("cloud")  ,"\n\n"))

  } # END of the loop over col.int


  #new.list <- list()
  cat("\nChecking for columns called", crayon::cyan( paste(columns.R, sep = ",")), "\n")

  for( col.int in columns.R){

    for(x in names(data)){

      tab.int <- data[[x]]

      if(col.int %in% names(tab.int)){

        cat("\nA column", col.int, "was observed in the table", crayon::cyan(x), "\n")

        tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_replace_all(" ", "_")  %>% stringr::str_replace_all("-", "_") %>%  as.character()
        tab.int[which( tab.int[, col.int] == "NA") , col.int] <- NA

        # Check pour les NA

        n.NA <- nrow(tab.int[which( is.na(tab.int[, col.int])) , col.int])

        if(n.NA > 0){

          answer <- NULL
          answer <- menu(title = paste("\n", n.NA, "missing values were observed. What should the primer be ? (0 to cancel corrections)" ),
                         graphics = F,
                         choice = amorceR
                         #choice = c(crayon::red("Missing value"), Projet)
          )

          #  if(answer == 1) { # Change to a real missing value
          #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
          #              cat(j, "was replaced with NA's\n")
          #            }

          if(answer %in% 1:(length(amorceR))){

            new.value <- Projet[[answer]]

            tab.int[which( is.na(tab.int[, col.int])) , col.int] <- new.value

            cat("NA was replaced with", new.value, "\n")

          }

          if(answer == 0){
            cat("No replacement done. Please change NA manually as no missing value should remained. \n")

          }

        }

        observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
        observed.vec <-  observed.vec[!is.na( observed.vec )]


        cat("\n Observed values are", crayon::cyan(paste(observed.vec, collapse = ", ")), "\n")

        if(length(observed.vec) > 0)  {  # only if there are values

          # Loop over unique value observed
          for(j in observed.vec){

            if((j %in% amorceR) == F){

              answer <- NULL
              answer <- menu(title = paste("\nWhat value should", crayon::cyan(j), "be ? (0 to cancel corrections)" ),
                             graphics = F,
                             choice = amorceR
                             #choice = c(crayon::red("Missing value"), Projet)
              )

              #  if(answer == 1) { # Change to a real missing value
              #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
              #              cat(j, "was replaced with NA's\n")
              #            }

              if(answer %in% 1:(length(amorceR))){

                new.value <- Projet[[answer]]

                tab.int[which(tab.int[, col.int] == j) , col.int] <- new.value

                cat(j, "was replaced with", new.value, "\n")

              }

              if(answer == 0){
                cat("No replacement done. Please add this values to the primer table prior to the importation. \n")

              }


            } # END of loop went something wrong is observed


          }   # END of loop over unique value observed

        } #else { cat(crayon::red("no data within this column\n"))}
        # END of if values observed
        ##
        # Check missing values

        cat( "\n", crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

             nrow(tab.int), "values)\n")

      }# END of loop over column name

      #  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
      #  return(new.list)
      data[[x]] <-  tab.int

    }   # END of the loop over table



    cat(crayon::green("\n\n", col.int , "columns were corrected", emojifont::emoji("cloud")  ,"\n\n"))

  } # END of the loop over col.int

  #new.list <- list()
  cat("\nChecking for columns called", crayon::cyan( paste(columns.P, sep = ",")), "\n")

  for( col.int in columns.P){

    for(x in names(data)){

      tab.int <- data[[x]]

      if(col.int %in% names(tab.int)){

        cat("\nA column", col.int, "was observed in the table", crayon::cyan(x), "\n")

        tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_replace_all(" ", "_")  %>% stringr::str_replace_all("-", "_") %>%  as.character()
        tab.int[which( tab.int[, col.int] == "NA") , col.int] <- NA

        # Check pour les NA

        n.NA <- nrow(tab.int[which( is.na(tab.int[, col.int])) , col.int])

        if(n.NA > 0){

          answer <- NULL
          answer <- menu(title = paste("\n", n.NA, "missing values were observed. What should the primer be ? (0 to cancel corrections)" ),
                         graphics = F,
                         choice = amorceP
                         #choice = c(crayon::red("Missing value"), Projet)
          )

          #  if(answer == 1) { # Change to a real missing value
          #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
          #              cat(j, "was replaced with NA's\n")
          #            }

          if(answer %in% 1:(length(amorceP))){

            new.value <- Projet[[answer]]

            tab.int[which( is.na(tab.int[, col.int])) , col.int] <- new.value

            cat("NA was replaced with", new.value, "\n")

          }

          if(answer == 0){
            cat("No replacement done. Please change NA manually as no missing value should remained. \n")

          }

        }

        observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
        observed.vec <-  observed.vec[!is.na( observed.vec )]


        cat("\n Observed values are", crayon::cyan(paste(observed.vec, collapse = ", ")), "\n")

        if(length(observed.vec) > 0)  {  # only if there are values

          # Loop over unique value observed
          for(j in observed.vec){

            if((j %in% amorceP) == F){

              answer <- NULL
              answer <- menu(title = paste("\nWhat value should", crayon::cyan(j), "be ? (0 to cancel corrections)" ),
                             graphics = F,
                             choice = amorceP
                             #choice = c(crayon::red("Missing value"), Projet)
              )

              #  if(answer == 1) { # Change to a real missing value
              #              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))
              #              cat(j, "was replaced with NA's\n")
              #            }

              if(answer %in% 1:(length(amorceP))){

                new.value <- Projet[[answer]]

                tab.int[which(tab.int[, col.int] == j) , col.int] <- new.value

                cat(j, "was replaced with", new.value, "\n")

              }

              if(answer == 0){
                cat("No replacement done. Please add this values to the primer table prior to the importation. \n")

              }


            } # END of loop went something wrong is observed


          }   # END of loop over unique value observed

        } #else { cat(crayon::red("no data within this column\n"))}
        # END of if values observed
        ##
        # Check missing values

        cat( "\n", crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

             nrow(tab.int), "values)\n")

      }# END of loop over column name

      #  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
      #  return(new.list)
      data[[x]] <-  tab.int

    }   # END of the loop over table



    cat(crayon::green("\n\n", col.int , "columns were corrected", emojifont::emoji("cloud")  ,"\n\n"))

  } # END of the loop over col.int


  return(data)

} # END of the function



#' @title Check the Index columns specifically
#'
#' @description
#' Function to check that index are in the right format
#'
#' @details
#'
#' @param data List containing important tables.
#' @param DB Name of the DB in the ODBC.
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_index  <- function(data, DB = "LabGeno"){

  col.int <-   "No_unique_index"

  model.index <- load_DB(table = "20_Index_librairies", DB = DB)

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  index <-  model.index  %>% dplyr::pull(col.int)

  columns.index <- model.other %>%
    dplyr::filter(Type == "index")  %>% pull(Col)

  #new.list <- list()
  cat("\nChecking for columns called", crayon::cyan( paste(columns.index, sep = ",")), "\n")

  for( col.int in columns.index){

    for(x in names(data)){

      tab.int <- data[[x]]

      if(col.int %in% names(tab.int)){

        cat("\nA column", col.int, "was observed in the table", crayon::cyan(x), "\n")

        for(i in 1:nrow(tab.int)){

          value.obs <- tab.int[i, col.int]
          value.exp <- paste(tab.int[i, "Kit_librairie"], tab.int[i, "Set_index"], tab.int[i, "Version_index"],  tab.int[i, "Puits_index"], sep = "_")

          if(is.na(value.obs) & (value.exp %in% index) ){

            tab.int[i, col.int] <-  value.exp

            cat( "\nA missing value was replaced by", value.exp,".\n" )

          } else if((value.obs == value.exp) & !(value.exp %in% index)  ) {

            cat( "\nThe observed value", crayon::red(value.obs), "is not in the current index table, a key violation will be encoutered. Please also revise please revise the columns Kit, Set, Version and Puit.\n" )

          } else if((value.exp %in% index) & !(value.obs %in% index) ){

            cat( "\nThe observed value", value.obs, "was replaced by", value.exp, "\n" )

            tab.int[i, col.int] <-  value.exp

          } else if(value.obs != value.exp){

            cat( "\nThe observed value", crayon::red(value.obs), "doesn't fit with expectation, please revise the columns Kit, Set, Version and Puit.\n" )

          } else if (!(value.observed %in% index)){

            cat( "\nThe observed value", crayon::red(value.obs), "doesn't fit with expectation, please revise the columns Kit, Set, Version and Puit.\n" )

          }

        }

        # Check missing values

        cat( "\n", crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

             nrow(tab.int), "values)\n")

      }# END of loop over column name

      #  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
      #  return(new.list)
      data[[x]] <-  tab.int

    }   # END of the loop over table



    cat(crayon::green("\n\n", col.int , "columns were checked", emojifont::emoji("key")  ,"\n\n"))

  } # END of the loop over col.int


  return(data)

} # END of the function



