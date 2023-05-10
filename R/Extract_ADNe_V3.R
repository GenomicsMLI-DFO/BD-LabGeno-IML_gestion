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

   tab.int <- data[["qPCR_ADNe"]] %>% dplyr::select(col.to.extract)

   if(names(tab.int) !=  col.to.extract){
     stop(paste("\nThe column names in qPCR_ADNe doesn't perfectly fit the one expected. Changes should be done manually, sorry ..."))

   names(tab.int) <- col.new.name

   tab.int <- tab.int %>% dplyr::filter(!is.na(Resultat_sequencage))

   # Message if nothing left

   if( nrow(tab.int == 0)){

   cat(crayon::orange("\nThe sheet Sequencage_Sanger_ADNe was created but was empty. In the end, no changes were made.\n\n"))

   } else{

   data[["Sequencage_Sanger_ADNe"]] <- tab.int

   cat(crayon::green("\nThe sheet Sequencage_Sanger_ADNe was created as a data frame of", ncol(tab.int), "columns and", nrow(tab.int), "row.", emojifont::emoji("rainbow")  ,"\n\n"))

   return(data)


   }


  }
   } # End of the function


