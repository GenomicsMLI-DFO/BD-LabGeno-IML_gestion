#' @title Check the Projet column specificallyr
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

correct_project  <- function(data){

  col.int <-   "Nom_projet"

  #model.ID   <-  suppressMessages(readxl::read_excel("inst/BD_format.xlsx", sheet = "ID"))
  Projet <-  model.projet  %>% pull(col.int)
  Projet <- Projet[order(Projet)]
  #new.list <- list()
  cat("\nChecking for columns called", crayon::cyan( col.int), "\n")

  for(x in names(data)){

    tab.int <- data[[x]]

    if(col.int %in% names(tab.int)){

     cat("\nA column", col.int, "was observed in the table", crayon::cyan(x), "\n")

     tab.int[, col.int] <- tab.int %>% dplyr::pull(col.int) %>% stringr::str_replace_all(" ", "_") %>% as.character()
     tab.int[which( tab.int[, col.int] == "NA") , col.int] <- NA

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
                           choice = c(crayon::red("Missing value"), Projet)
            )

            if(answer == 1) { # Change to a real missing value

              tab.int[which(tab.int[, col.int] == j) , col.int]  <- NA #  rep(NA, nrow(tab.int))

              cat(j, "was replaced with NA's\n")

            }

            if(answer %in% 2:(length(Projet)+1)){

              new.value <- Projet[[answer-1]]

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

