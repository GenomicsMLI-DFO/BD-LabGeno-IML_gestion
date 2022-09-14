#' @title Upload Excel template
#'
#' @description
#' Function to upload an Excel file based on the predefined template
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data List containing important tables.
#' @param skip.col = 1
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

check_column_name  <- function(data,
                               skip.col = 1){

  model <-  suppressMessages(readr::read_csv("inst/BD_columns_format.csv"))

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

   cat(ncol(tab.int) - skip.col, "columns were uploaded (", length(model.vec), "were expected )\nNow looking column by column for the good name and order ... \n\n")

   # Check column names one by one

   tab.new <- tibble::tibble(.rows = nrow(tab.int))

   for(i in 1:length(model.vec)){

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






