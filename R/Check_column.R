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
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_name  <- function(data
                               ){
  #model.ordre

  new.list <- list()

  for(x in names(data)){

  cat("\nChecking column names for", crayon::cyan(x), "\n")

  model.int <- model.ordre %>% dplyr::filter(Table == x) %>% dplyr::select(-Table)

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

     answer <- menu(title = paste("\nWas the column", crayon::cyan(model.vec[[i]]), "called another name or it's a new column?" ),
                 graphics = F,
                 choice = c(crayon::green("ADD as a new column"), names(tab.int) )
     )

     #
     if(answer == 1) {

     tab.new[model.vec[[i]]]  <- NA #  rep(NA, nrow(tab.int))

     cat("\nA new column", model.vec[[i]], "was created.\n\n" )

     }

     if(answer %in% 2:(length( names(tab.int))+1)){

    new.row <- tab.int[,  names(tab.int)[answer-1]]
    tab.new <- dplyr::bind_cols(tab.new, new.row)

    names(tab.new)[ncol(tab.new)] <- model.vec[[i]]

    cat("\nThe columns",  names(tab.int)[answer-1], "was renamed", model.vec[[i]], ".\n\n" )


    }

   }


   }# END of the LOOP over column names

   #
   if(all(names(tab.new) == model.vec[,] )) {

     cat(crayon::green("\nThe table", x, "now have the right column names and order\n"))

     new.list[[x]] <- tab.new



   } else  {

     cat(crayon::red("SOMETHING WENT WRONG"))

   }


   } # END of the loop over one table

  }

  cat(crayon::green("\nThe sheets", paste(names(new.list), collapse = ", "), "now have the right columns", emojifont::emoji("champagne")  ,"\n\n"))

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
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

check_column_values_ID  <- function(data){

  for(x in names(data)){

    cat("\nChecking column values for", crayon::cyan(x), "\n")

    tab.int <- data[[x]]

    model.ID.int <- model.ID %>% dplyr::filter(Col %in% names(tab.int))

   # model.list.int <- model.list %>% dplyr::filter(Tab == x) #%>% dplyr::select(-Table)

    # Do it only if a column exist

    if(nrow(model.ID.int) > 0){

    for(i in 1:ncol(tab.int)){ # Loop over each column

      col.int <-names(tab.int)[i]


      # Si ça doit suivre un model
      if(col.int %in% model.ID.int$Col){

      cat(col.int, ": ")

      tab.int[which(tab.int[, col.int] == "NA"), col.int] <- NA

      # Check missing values
      if( all(!is.na(tab.int[,col.int])) == T){
         cat("No missing values observed, ")

      } else cat(crayon::red("Missing values observed, "))


      unique.vec <- unique(dplyr::pull(tab.int[ ,  col.int]))
      unique.vec <- unique.vec[!is.na(unique.vec)]

      # Loop over each valuesb

      if(length(unique.vec > 0))  {  # only if there are values

        bad.j <- vector()
        for(j in  unique.vec){

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
          cat(crayon::red("some values are problematics :", paste(bad.j, collapse = ", "), " \n\n"))

        } else cat("all in the good format\n")
      } else { cat(crayon::red("no data within this column\n"))}

    }

  }# END of loop over column name

    } # END of IF you can do it

#  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
#  return(new.list)

}   # END of the loop over table

    cat(crayon::green("\nEnd of the verification, please check potential problems carefully", emojifont::emoji("four_leaf_clover")  ,"\n\n"))
  #  return(new.list)


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
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_values_factor  <- function(data){

  for(x in names(data)){

    cat("\nChecking column values for", crayon::cyan(x), "\n")

    tab.int <- data[[x]]

    model.list.int <- model.list %>% dplyr::filter(Col %in% names(tab.int))

    # model.list.int <- model.list %>% dplyr::filter(Tab == x) #%>% dplyr::select(-Table)

    if(nrow(model.list.int) > 0) { #DO IT ONLY IF THERE ARE SOME VALUE AVAILABLE



    for(i in 1:nrow(model.list.int)){ # Loop over each column

      col.int <-   model.list.int[[i,"Col"]]# %>% as.vector()

      factor.vec <-  model.list.int[i,as.vector(!is.na(model.list.int[i,]))] %>% dplyr::select(-"Col") %>% as.data.frame()
      factor.vec <- suppressWarnings(factor.vec[1,order(factor.vec[1,])])

      tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_replace_all(" ", "_") %>% as.character()

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()

      observed.vec <-  observed.vec[!is.na( observed.vec )]

      # Test si c'est numérique

      observed.vec.num <- suppressWarnings(as.numeric(observed.vec))

      # If not numeric
      if(all(!is.na(observed.vec.num ))){

      cat("\n", col.int, ": Numeric value observed, converted as round charecter.\n")

      observed.vec <- as.character(round(observed.vec.num,0))

      tab.int[ , col.int]  <- suppressWarnings(as.character(round(as.numeric( tab.int %>% pull(col.int)))))

       }

      cat("\n", col.int, ": predefined values are", crayon::green(paste(factor.vec, collapse = ", ")),
          "\n Observed values are", crayon::cyan(paste(observed.vec, collapse = ", ")), "\n")

      if(length(observed.vec) > 0)  {  # only if there are values

      # Loop over unique value observed
       for(j in observed.vec){

         if((j %in% factor.vec) == F){

          answer <- NULL
          answer <- menu(title = paste("\nWhat value should", crayon::cyan(j), "be ? (0 to cancel corrections and keep current value)" ),
                          graphics = F,
                          choice = c(crayon::red("Missing value"), crayon::green("New value"), factor.vec )
          )

          if(answer == 1) { # Change to a real missing value

          tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))

          cat(j, "was replaced with NA's\n")

          }

          if(answer == 2) { # Change to a new value

            answer2 <- NULL
            answer2 <- readline(prompt = paste(crayon::white("Which value should it be? Please add this value to the list of expected values.  "  )))

            #answer <- as.numeric(answer)

            if(!is.na(answer2) && !is.null(answer2)) { # Change to a real missing value

              tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer2 #  rep(NA, nrow(tab.int))

              cat(j, "was replaced with", answer2, "\n")

            } else { cat ("Something went wrong")}
          }



          if(answer %in% 3:(length( factor.vec)+2)){

           new.value <- factor.vec[[answer-2]]

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

    } # END of if there are some values available

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
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_values_date  <- function(data){

  for(x in names(data)){

    cat("\nChecking column values for", crayon::cyan(x), "\n")

    tab.int <- data[[x]]

    model.date.int <- model.date %>% dplyr::filter(Col %in% names(tab.int))



    if(nrow(model.date.int)>0){ # Add the exception to do it only if dates exist

    for(i in 1:nrow(model.date.int)){ # Loop over each column

      col.int <-   model.date.int[[i,"Col"]]# %>% as.vector()
      tab.int[which(tab.int[, col.int] == "NA"), col.int] <- NA
      tab.int[which(tab.int[, col.int] == "ND"), col.int] <- NA

      date.range <-  model.date.int[i,]%>% dplyr::select(-"Col")# %>% as.vector()

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]

      cat("\n", col.int, ":\n")


      # Loop over values observed
        for(j in observed.vec){

        test.num <- as.numeric(j)

          # If not numeric
        if(is.na(test.num)){

            answer <- NULL
            answer <- readline(prompt = paste(crayon::white("\nThe observed value", crayon::cyan(j), "is not numeric, which value should it be? Values between", date.range[1], "and", date.range[2], "are expected. NA values are accepted."  )))

            #answer <- as.numeric(answer)


            if(!is.na(answer) && !is.null(answer)) { # Change to a real missing value

              tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer #  rep(NA, nrow(tab.int))

              cat(j, "was replaced with", answer, "\n")

            } else { cat ("The observed value was not replaced.")}
            }


        # If numeric
        if(!is.na(test.num)){

          # Should be in the right range

          if( (test.num %in%  date.range[[1]]:date.range[[2]]) == F){

          answer <- NULL
          answer <- readline(paste(crayon::white("The observed value", crayon::inverse(j), "is not in the expected range (between", date.range[1], "and", date.range[2], "). What value should it be? NA values are accepted. "  )))

          #answer <- as.numeric(answer)

          if(answer == "NA"){

          tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))

          cat(j, "was replaced with NA\n")


          } else if(!is.na(answer) && !is.null(answer)) { # Change to a real missing value

            tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer #  rep(NA, nrow(tab.int))

            cat(j, "was replaced with", answer, "\n")

          } else { cat ("The observed value was not replaced.")}

          } else{ # Do a round

            tab.int[which(tab.int[, col.int] == j) , col.int] <- as.character(round(test.num, 0))

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

    } # END the exception to do it only if dates exist

  }   # END of the loop over table


  cat(crayon::green("\n\nColumns with date format were corrected", emojifont::emoji("apple")  ,"\n\n"))
  return(data)

} # END of the function


#' @title Check and correct column with hour
#'
#' @description
#' Function to check that column with hour are in the right format, and do a correction if needed.
#'
#' @details
#' Character : will be changed for h
#'
#' @param data List containing important tables.
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

correct_column_values_hour  <- function(data){

  model.hour <-  model.other %>% dplyr::filter(Type == "heure")

  for(x in names(data)){

    cat("\nChecking column values for", crayon::cyan(x), "\n")

    tab.int <- data[[x]]

    model.hour.int <- model.hour %>% dplyr::filter(Col %in% names(tab.int))

    if(nrow( model.hour.int)>0){ # Do something only if you need too

      for(i in 1:nrow(model.hour.int)){ # Loop over each column


        col.int <-   model.hour.int[[i,"Col"]]# %>% as.vector()

        tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int)
        tab.int[which(tab.int[, col.int] == "NA"), col.int] <- NA

        observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
        observed.vec <-  observed.vec[!is.na( observed.vec )]

        cat("\n", col.int, ":\n")

        # do something only if there are values

        if(length(observed.vec)>0){

          # Loop over values observed
          for(j in observed.vec){

            hour.num <- as.numeric(j)

            # If  numeric and between 0 and 1
            if(!is.na(hour.num) & dplyr::between(hour.num, 0, 1)){

              new.value <- as.character(hms::hms(days = hour.num)) %>% stringr::str_sub(1,5)  %>%
                                                                   stringr::str_replace(":", "h")

              tab.int[which(tab.int[, col.int] == j) , col.int] <- new.value

              cat(j, "was replaced with",new.value, "\n")
            } else

            # If numeric between 1 and 24
            if(!is.na(hour.num) & dplyr::between(hour.num, 1, 24)){
              new.value = paste0(hour.num, "h00")

              tab.int[which(tab.int[, col.int] == j) , col.int] <- new.value

              cat(j, "was replaced with",new.value, "\n")

            } else

            if(nchar(j) > 5 | !stringr::str_detect(j, "h")){

              answer <- NULL
              answer <- readline(prompt = paste(crayon::white("The observed value", crayon::red(j), "doesn't fit the right format (i.e., 00h00),which value should it be? "  )))


              if(!is.na(answer) && !is.null(answer)) { # Change to a real missing value

                tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer #  rep(NA, nrow(tab.int))

                cat(j, "was replaced with", answer, "\n")

              } else { cat ("Something went wrong")}





            } # ALLL CHECKS





          }   # END of loop over unique value observed
          # Convert as numeric

        } else { # if there is not data

          cat(crayon::red("no data within this column"), ", ")

        }

        # END of if values observed
        ##
        # Check missing values

        cat(crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

            nrow(tab.int), "values)\n")

      }# END of loop over column name

      data[[x]] <-  tab.int

    }  else { cat("\nNo column with hour format in this sheet\n")} # Do something only if you need too
  }# END of the loop over table

cat(crayon::green("\n\nColumns with hour format were corrected", emojifont::emoji("last_quarter_moon_with_face")  ,"\n\n"))
return(data)

} # END of the function




#' @title Check and correct column with numeric values within
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
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_values_numeric  <- function(data){

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  model.num <-  model.other %>%
                dplyr::filter(Type == "numeric")

  #new.list <- list()

  for(x in names(data)){

    cat("\nChecking numeric columns for", crayon::cyan(x), "\n")

    tab.int <- data[[x]]

    model.num.int <- model.num %>% dplyr::filter(Col %in% names(tab.int))

    if(nrow(model.num.int)>0){ # Add the exception to do it only if dates exist

    for(i in 1:nrow(model.num.int)){ # Loop over each column

      col.int <-   model.num.int[[i,"Col"]]# %>% as.vector()

      tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_trim()
      tab.int[which(tab.int[, col.int] == "NA"), col.int] <- NA
      tab.int[which(tab.int[, col.int] == "ND"), col.int] <- NA

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]

      cat("\n", col.int, ":\n")

     # do something only if there are values

      if(length(observed.vec)>0){

      # Loop over values observed
      for(j in observed.vec){

        test.num <- suppressWarnings(as.numeric(j))

        # If not numeric
        if(is.na(test.num)){

          # Try to change automatically "," to "."

          test.num.corrected <- suppressWarnings(as.numeric(j %>% stringr::str_replace(",", ".") ))

          if(!is.na(test.num.corrected)){

          tab.int[which(tab.int[, col.int] == j) , col.int]  <- as.character(test.num.corrected) #  rep(NA, nrow(tab.int))
          cat(j, "was replaced with",  test.num.corrected, "automatically.\n")

          }

          if(is.na(test.num.corrected)){
          answer <- NULL
          answer <- readline(prompt = paste(crayon::white("The observed value", crayon::red(j), "is not numeric, which value should it be? "  )))

          answer #<- as.numeric(answer)


          if(!is.na(answer) && !is.null(answer)) { # Change to a real missing value

            tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer #  rep(NA, nrow(tab.int))

            cat(j, "was replaced with", answer, "\n")

          } else { cat ("Something went wrong")}
         }
        }
      }   # END of loop over unique value observed
      # Convert as numeric

      tab.int[, col.int] <- suppressWarnings(as.numeric(as.character( tab.int %>% dplyr::pull(col.int))))

      cat( "\n range of observation between", tab.int %>% dplyr::pull(col.int) %>% min(na.rm = T), "and",
           tab.int %>% dplyr::pull(col.int) %>% max(na.rm = T), ", ")

      } else { # if there is not data

        cat(crayon::red("no data within this column"), ", ")

      tab.int[, col.int] <- suppressWarnings(as.numeric(as.character( tab.int %>% dplyr::pull(col.int))))

      }

      # END of if values observed
      ##
      # Check missing values

      cat(crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

           nrow(tab.int), "values)\n")

    }# END of loop over column name

    #  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
    #  return(new.list)

    data[[x]] <-  tab.int

    } # END the exception to do it only if num columns exist

  }   # END of the loop over table


  cat(crayon::green("\n\nColumns with numeric format were corrected", emojifont::emoji("snake")  ,"\n\n"))
  return(data)

} # END of the function



#' @title Check and correct column with a well number
#' #'
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
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_values_well  <- function(data){

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  model.well <- model.other %>%
    dplyr::filter(Type == "puit")

  #new.list <- list()

  for(x in names(data)){

    cat("\nChecking well columns for", crayon::bgMagenta(x), "\n")

    tab.int <- data[[x]]

    model.well.int <- model.well %>% dplyr::filter(Col %in% names(tab.int))

    if(nrow( model.well.int)>0){ # Do something only if you need too

    for(i in 1:nrow(model.well.int)){ # Loop over each column


      col.int <-   model.well.int[[i,"Col"]]# %>% as.vector()

      tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_trim()
      tab.int[which(tab.int[, col.int] == "NA"), col.int] <- NA

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]

      cat("\n", col.int, ":\n")

      # do something only if there are values

      if(length(observed.vec)>0){

        # Loop over values observed
        for(j in observed.vec){

          Range.well <- stringr::str_sub(j, 1,1) #%>% stringr::str_to_upper()

          Col.well   <- suppressWarnings( j %>% stringr::str_remove(Range.well) %>% as.numeric() )

          Col.well.double <- ifelse(Col.well < 10, paste0(0, Col.well), Col.well)

          New.well <- paste0(stringr::str_to_upper(Range.well), Col.well.double)

          # If not numeric
          if(is.na(Col.well)){

            answer <- NULL
            answer <- readline(prompt = paste(crayon::white("The observed value", crayon::red(j), "doesn't fit the right format (i.e., A1 to H12),which value should it be? "  )))


            if(!is.na(answer) && !is.null(answer)) { # Change to a real missing value

              tab.int[which(tab.int[, col.int] == j) , col.int]  <- answer #  rep(NA, nrow(tab.int))

              cat(j, "was replaced with", answer, "\n")

            } else { cat ("Something went wrong")}

          } else if(New.well != j){

            cat(j, "was replaced with", New.well, "\n")

            tab.int[which(tab.int[, col.int] == j) , col.int]  <- New.well #  rep(NA, nrow(tab.int))

          }

        }   # END of loop over unique value observed
        # Convert as numeric

      } else { # if there is not data

        cat(crayon::red("no data within this column"), ", ")

      }

      # END of if values observed
      ##
      # Check missing values

      cat(crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

          nrow(tab.int), "values)\n")

    }# END of loop over column name

    data[[x]] <-  tab.int

    }  else { cat("\nNo column with well format in this sheet\n")} # Do something only if you need too



  }  # END of the loop over table


  cat(crayon::green("\n\nColumns with well format were corrected", emojifont::emoji("first_quarter_moon_with_face")  ,"\n\n"))
  return(data)

} # END of the function



#' @title Check and correct all the remaining columns
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
#' @seealso [correct_column_name()] to create the list of tables.
#'
#` @references
#' List references
#' @export

correct_column_values_others  <- function(data){

  col.not.other <- c(model.list, model.ID, model.date, model.other)

  #new.list <- list()

  for(x in names(data)){

    cat("\nChecking other columns for", crayon::cyan(x), "\n")

    tab.int <- data[[x]]

    model.other <-  names(tab.int)[!(names(tab.int) %in% col.not.other) ]

    for(i in model.other){ # Loop over each column

      col.int <- i

      tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_trim() #%>% as.factor()
      tab.int[which(tab.int[, col.int] == "NA"), col.int] <- NA

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]

      cat("\n", col.int, ":\n")

      # REMOVE ALL SPECIAL CARACTERS

      if(length(observed.vec) > 1){

        cat("\n Looking for special characters\n\n")

        for(j in observed.vec){
         new.value <- NULL
         test.num <-  suppressWarnings(as.numeric(j))

         if(!is.na(test.num)){
         new.value <- as.character(test.num)
         }

         if(is.na(test.num)){
           new.value <- remove_all_special(j)
         }

         #
         if(new.value != j){

         tab.int[which(tab.int[, col.int] == j), col.int] <- new.value

         cat(" ", j, "was replaced by", new.value, "\n")
         }


        } # End of loop over unique values

      } # END of special character correction

      observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
      observed.vec <-  observed.vec[!is.na( observed.vec )]


      # do something only if there are values

      if(length(observed.vec)>0){

        N.max <- tab.int[, col.int] %>% table() %>% max()
        N.min <- tab.int[, col.int] %>% table() %>% min()

        N.unique <- tab.int[which(!is.na(tab.int[, col.int])), col.int] %>% unique() %>% nrow()

        #N.na <- nrow(tab.int[which(is.na(tab.int[, col.int])), col.int])


        cat("\n", length(observed.vec), "values observed, repeated betwen", N.min, "and", N.max,
            "times, with", N.unique, "unique values.\n")

        answer <- NULL
        answer <- menu(title = paste("\nDo you want to check all values observed?" ),
                       graphics = F,
                       choice = c("Yes", "No")
        )

        if(answer == 1) { # Change to a real missing value

          print(as.character(observed.vec))

        # Ask a new question <-

          answer2 <- 1

          while(answer2 == 1) {
           answer2 <- menu(title = paste("\nDo you want to modify one value?" ),
                         graphics = F,
                         choice = c("Yes, ask one by one",
                                    "No, everything looks fine"))
           # Then ask one by one
           if(answer2 == 1){
              answer3 <- NULL
              answer3 <- menu(title = paste("\nWhich one you want to modify? (0 to exit this menu)" ),
                              graphics = F,
                              choice = c(as.character(observed.vec)))

              if(answer3 != 0){

                new.value <- NULL
                new.value <- readline(prompt = paste("Which value",as.character(observed.vec)[answer3], "should be? "  ))

                tab.int[which(tab.int[, col.int] == as.character(observed.vec)[answer3]), col.int] <- new.value
                cat("\n", as.character(observed.vec)[answer3], "was changed to", new.value, "\n\n")

                # Update the vector

                observed.vec <- tab.int %>% dplyr::pull(col.int) %>% unique()
                observed.vec <-  observed.vec[!is.na( observed.vec )]

                # Print the updated values

                print(as.character(observed.vec))
              } # END of the BIG loop of correction

            }
           if(answer2 == 2){

             cat("\n Correction mode is over\n")
           }
           }

         } # End of first answer
      }
      # END of if values observed
      ##
      # Check missing values

      cat(crayon::red(nrow(tab.int[which(is.na(tab.int[,col.int])),col.int])) ,  "missing values observed (over",

          nrow(tab.int), "values)\n")

    }# END of loop over column name

    #  cat(crayon::green("The values within the sheets", paste(names(new.list), collapse = ", "), "were checked", emojifont::emoji("smile")  ,"\n\n"))
    #  return(new.list)

    data[[x]] <-  tab.int

  }   # END of the loop over table


  cat(crayon::green("\n\nRemaining Columns were corrected", emojifont::emoji("beer")  ,"\n\n"))
  return(data)

} # END of the function



