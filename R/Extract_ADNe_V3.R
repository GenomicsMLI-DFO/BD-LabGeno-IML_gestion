#' @title Extract old eDNA gabarit SANGER sequencing info
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

extract_sequencing_ADNe  <- function(data){

   # List the columns we are looking to

   col.to.extract <-  model.eDNA_convert %>% dplyr::filter(Table_ORIGINAL == "qPCR_ADNe",
                                                           Table_NEW == "Sequencage_Sanger_ADNe") %>%
                                             pull(Column_ORIGINAL)
   col.new.name <-  model.eDNA_convert %>% dplyr::filter(Table_ORIGINAL == "qPCR_ADNe",
                                                           Table_NEW == "Sequencage_Sanger_ADNe") %>%
     pull(Column_NEW)

   # Stop if not qPCR_ADNe detected
   if(is.null(data[["qPCR_ADNe"]])){
     stop("\nThe sheet qPCR_ADNe is not present in your data.frame, columns cannot be extracted.")
   }

   # Stop if Sequencage_Sanger_ADNe detected
   if(!is.null(data[["Sequencage_Sanger_ADNe"]])){
     stop("\nThe sheet Sequencage_Sanger_ADNe is already present, columns cannot be extracted.")
   }

   tab.int <- data[["qPCR_ADNe"]] %>% dplyr::select(dplyr::all_of(col.to.extract))

   if(all(names(tab.int) !=  col.to.extract)){
     stop(paste("\nThe column names in qPCR_ADNe doesn't perfectly fit the one expected. Changes should be done manually, sorry ..."))
     }
   names(tab.int) <- col.new.name

   tab.int <- tab.int %>% dplyr::filter(!is.na(Sequence_qPCR_ADNe))

   # Message if nothing left

   if( nrow(tab.int) == 0){

   cat(crayon::red("\nThe sheet Sequencage_Sanger_ADNe was created but was empty (no sequence available). No changes were made.\n\n"))

   } else{

   data[["Sequencage_Sanger_ADNe"]] <- tab.int

   cat(crayon::green("\nThe sheet Sequencage_Sanger_ADNe was created as a data frame of", ncol(tab.int), "columns and", nrow(tab.int), "row.", emojifont::emoji("rainbow")  ,"\n\n"))

   }

   return(data)


   } # End of the function




#' @title Check column names from V3 format
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

precorrect_ADNe_V3_column_name <- function(data
){
  #model.ordre

  new.list <- list()

  for(x in names(data)){

    cat("\nPre-correcting column names for", crayon::cyan(x), "when possible\n")

    model.int <- model.eDNA_convert %>% dplyr::filter(Table_NEW == x)

    tab.int <- data[[x]]

    # Don't do anything if you cannot read the format file
    if(nrow(model.int) == 0){
      cat(crayon::red("\nWARNING: No automatic correcion available!!\n"))
    }

    # Do something if you can read the format file
    if(nrow(model.int) >0){

      model.vec.ORI <-  model.int$Column_ORIGINAL
      model.vec.NEW <-  model.int$Column_NEW

      cat(ncol(tab.int), "columns were uploaded (", length(model.vec.ORI), "could potentially be automatically renamed)\nNow looking column by column to see if something magic can happen ... \n\n")

      # Check column names one by one

      for(i in 1:length(model.vec.ORI)){ # Loop over each column

        # Do something if you encounter the right name
        if(model.vec.ORI[i] %in% names(tab.int)){

          # Change the name
          names(tab.int)[names(tab.int) ==model.vec.ORI[i]] <- model.vec.NEW[i]

        cat("\nThe columns", model.vec.ORI[[i]], "was renamed", model.vec.NEW[[i]] )

        }
      }# END of the LOOP over column names

      data[[x]] <-   tab.int
       cat(crayon::green("\nThe table", x, "is now corrected\n"))


    } # END of the loop over one table

  }

  cat(crayon::green("\nIt's done", emojifont::emoji("chicken")  ,"\n\n"))

  return(data)

}


