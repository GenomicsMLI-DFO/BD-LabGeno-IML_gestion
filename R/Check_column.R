#' @title Check column names
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

correct_column_name  <- function(data
                               ){

  model <-   suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "Ordre"))

  new.list <- list()

  for(x in names(data)){

  cat("\nChecking column names for", crayon::bgMagenta(x), "\n")

  model.int <- model %>% dplyr::filter(Table == x) %>% dplyr::select(-Table)

  tab.int <- data[[x]]

  # Don't do anything if you cannot read the format file
  if(nrow(model.int) == 0){
    cat(crayon::red("\nWARNING : Something went wrong with reading gabarit column names!!\n"))
  }

  # Do something if you can read the format file
  if(nrow(model.int) == 1){

   model.vec <- model.int[1,as.vector(!is.na(model.int[1,]))]

   cat(ncol(tab.int), "columns were uploaded (", length(model.vec), "were expected )\nNow looking column by column for the good name and order ... \n\n")

   # Check column names one by one

   tab.new <- tibble::tibble(.rows = nrow(tab.int))

   for(i in 1:length(model.vec)){ # Loop over each column

    new.row <- NULL
    answer <- NULL

    cat(model.vec[[i]], "\n")

   # easiest part, if you encounter the right name
   if(model.vec[i] %in% names(tab.int)){

    new.row <- tab.int %>% dplyr::select(model.vec[[i]])

    tab.new <- dplyr::bind_cols(tab.new, new.row)

   } else {

     answer <- menu(title = paste("\nWas the column", crayon::inverse(model.vec[[i]]), "called another name or it's a new column?" ),
                 graphics = F,
                 choice = c(crayon::green("ADD as a new column"), names(tab.int) )
     )

     #
     if(answer == 1) {

    tab.new[model.vec[[i]]]  <- NA #  rep(NA, nrow(tab.int))

     }

     if(answer %in% 2:(length( names(tab.int))+1)){

    new.row <- tab.int[,  names(tab.int)[answer-1]]
    tab.new <- dplyr::bind_cols(tab.new, new.row)

    names(tab.new)[ncol(tab.new)] <- model.vec[[i]]
    }

   }


   }# END of the LOOP over column names

   #
   if(all(names(tab.new) == model.vec[,] )) {

     cat(crayon::green("\nThe table", x, "has now the right column names and order\n"))

     new.list[[x]] <- tab.new



   } else  {

     cat(crayon::red("SOMETHING WENT WRONG"))

   }


   } # END of the loop over one table

  }

  cat(crayon::green("The sheets", paste(names(new.list), collapse = ", "), "now have the right columns", emojifont::emoji("champagne")  ,"\n\n"))
  return(new.list)

  }



#' @title Check column values
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
#' @seealso [check_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

check_column_values_ID  <- function(data){

  model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  #model.list <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "Valeurs"))

  #new.list <- list()

  for(x in names(data)){

    cat("\nChecking column values for", crayon::bgMagenta(x), "\n")

    tab.int <- data[[x]]

    model.ID.int <- model.ID %>% dplyr::filter(Col %in% names(tab.int))

   # model.list.int <- model.list %>% dplyr::filter(Tab == x) #%>% dplyr::select(-Table)

    for(i in 1:ncol(tab.int)){ # Loop over each column

      col.int <-names(tab.int)[i]


      # Si ça doit suivre un model
      if(col.int %in% model.ID.int$Col){

      cat(col.int, ": ")

      # Check missing values
      if( all(!is.na(tab.int[,col.int])) == T){
         cat("No missing values observed, ")

      } else cat(crayon::red("Missing values observed, "))


      ## Check duplicated
      #if( all(!duplicated(tab.int[,col.int])) == T){
      #  cat("no duplicated values observed, ")
      #} else cat(crayon::yellow("duplicated values observed (", paste(unique(dplyr::pull(tab.int[ duplicated(tab.int[,col.int]),  col.int])), collapse = ", ")))
      #
      #
      #} # End of model check


      # Loop over each valuesb

      if(all(!is.na( unique(dplyr::pull(tab.int[ ,  col.int])))))  {  # only if there are values

        bad.j <- vector()
        for(j in  unique(dplyr::pull(tab.int[ ,  col.int]))){

          model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(Prefixe)

          test.j <- stringr::str_split(j, pattern = model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(Sep))

          if(test.j[[1]][1] != model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(Prefixe) |
             nchar(test.j[[1]][2]) !=  model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(N_Annee) |
             nchar(test.j[[1]][3]) !=  model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(N_Num) |
             nchar(j) !=  ((model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(Prefixe) %>% nchar()) + (model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(N_Annee)) + (model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(N_Num)) + (2 * ( model.ID.int %>% dplyr::filter(Col == col.int) %>% dplyr::pull(Sep)) %>% nchar()))

             ){

            bad.j <- c( bad.j , j)

            }

        } # loop over each values

        if(length(bad.j)>0){
          cat(crayon::red("some values are problematics ", paste(bad.j, collapse = ", "), " )\n"))

        } else cat("all in the good format\n")
      } else { cat(crayon::red("no data within this column\n"))}

    }

  }# END of loop over column name

#  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
#  return(new.list)

  }   # END of the loop over table

} # END of the function


#' @title Check column factor
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
#' @seealso [check_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_values_factor  <- function(data){

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  model.list <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "Valeurs"))

  #new.list <- list()

  for(x in names(data)){

    cat("\nChecking column values for", crayon::bgMagenta(x), "\n")

    tab.int <- data[[x]]

    model.list.int <- model.list %>% dplyr::filter(Col %in% names(tab.int))

    # model.list.int <- model.list %>% dplyr::filter(Tab == x) #%>% dplyr::select(-Table)

    for(i in 1:nrow(model.list.int)){ # Loop over each column


      col.int <-   model.list.int[[i,"Col"]]# %>% as.vector()

      factor.vec <-  model.list.int[i,as.vector(!is.na(model.list.int[i,]))] %>% dplyr::select(-"Col")# %>% as.vector()

      tab.int[, col.int] <- tab.int %>% pull(col.int) %>% stringr::str_replace_all(" ", "_") %>% as.character()

      observed.vec <- tab.int %>% pull(col.int) %>% unique()

      observed.vec <-  observed.vec[!is.na( observed.vec )]

      cat("\n", col.int, ": predefined values are", crayon::green(paste(factor.vec, collapse = ", ")),
          "\n Observed values are", crayon::inverse(paste(observed.vec, collapse = ", ")), "\n")

      if(length(observed.vec) > 0)  {  # only if there are values

      # Loop over unique value observed
       for(j in observed.vec){

         if((j %in% factor.vec) == F){

          answer <- NULL
          answer <- menu(title = paste("\nWhat value should", crayon::inverse(j), "be ? (0 to cancel corrections)" ),
                          graphics = F,
                          choice = c(crayon::red("Missing value"), factor.vec )
          )

          if(answer == 1) { # Change to a real missing value

          tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))

          cat(j, "was replaced with NA's\n")

          }

          if(answer %in% 2:(length( factor.vec)+1)){

           new.value <- factor.vec[[answer-1]]

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


  cat(crayon::green("\n\nColumns with predefined format were corrected", emojifont::emoji("watermelon")  ,"\n\n"))
  return(data)

} # END of the function



#' @title Check and correct column with Date
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
#' @seealso [check_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_values_date  <- function(data){

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  model.date <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "Date"))

  #new.list <- list()

  for(x in names(data)){

    cat("\nChecking column values for", crayon::bgMagenta(x), "\n")

    tab.int <- data[[x]]

    model.date.int <- model.date %>% dplyr::filter(Col %in% names(tab.int))

    for(i in 1:nrow(model.date.int)){ # Loop over each column


      col.int <-   model.date.int[[i,"Col"]]# %>% as.vector()

      date.range <-  model.date.int[i,]%>% dplyr::select(-"Col")# %>% as.vector()

      observed.vec <- tab.int %>% pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]

      cat("\n", col.int, ":\n")


      # Loop over values observed
        for(j in observed.vec){

        test.num <- as.numeric(j)

          # If not numeric
        if(is.na(test.num)){

            answer <- NULL
            answer <- readline(prompt = paste(crayon::white("\nThe observed value", crayon::inverse(j), "is not numeric, which value should it be? Values between", date.range[1], "and", date.range[2], "are expected. "  )))

            answer <- as.numeric(answer)


            if(!is.na(answer) && !is.null(answer)) { # Change to a real missing value

              tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer #  rep(NA, nrow(tab.int))

              cat(j, "was replaced with", answer, "\n")

            } else { cat ("Something went wrong")}
            }


        # If numeric
        if(!is.na(test.num)){

          # Should be in the right range

          if( (test.num %in%  date.range[[1]]:date.range[[2]]) == F){

          answer <- NULL
          answer <- readline(paste(crayon::white("\nThe observed value", crayon::inverse(j), "is not in the expected range (between", date.range[1], "and", date.range[2], "). WHat value should it be? "  )))

          answer <- as.numeric(answer)


          if(!is.na(answer) && !is.null(answer)) { # Change to a real missing value

            tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer #  rep(NA, nrow(tab.int))

            cat(j, "was replaced with", answer, "\n")

          } else { cat ("Something went wrong")}

          }

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


  cat(crayon::green("\n\nColumns with date format were corrected", emojifont::emoji("apple")  ,"\n\n"))
  return(data)

} # END of the function





