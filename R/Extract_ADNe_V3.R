#' @title Extract old eDNA gabarit SANGER sequencing info
#'
#' @description
#' Function to extract the Sanger sequencing info from the qPCR table, as encounter in the eDNA gabarit v3.
#'
#' @details
#' The data object should include a "qPCR_ADNe" table, but not a "Sequencage_Sanger_ADNe"
#'
#' @param data List containing important tables.
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso [upload_gabarit_ADNe()] to create the list of tables.
#'
#` @references
#' List references
#' @export

extract_sequencing_ADNe  <- function(data){

   # List the columns we are looking to

   col.to.extract <-  model.eDNA_convert %>% dplyr::filter(Table_ORIGINAL == "qPCR_ADNe",
                                                           Table_NEW == "Sequencage_Sanger_ADNe") %>%
                                             dplyr::pull(Column_ORIGINAL)
   col.new.name <-  model.eDNA_convert %>% dplyr::filter(Table_ORIGINAL == "qPCR_ADNe",
                                                           Table_NEW == "Sequencage_Sanger_ADNe") %>%
     dplyr::pull(Column_NEW)

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

   tab.int <- tab.int %>% dplyr::filter(!is.na(Sequence_qPCR_ADNe)) %>%
                          dplyr::mutate(Notes_sequencage_ADNe = "Automatically_created_with_extract_sequencing_ADNe_R_function",
                                        Modifications_sequencage_ADNe = NA)


   # Message if nothing left

   if( nrow(tab.int) == 0){

   cat(crayon::red("\nThe sheet Sequencage_Sanger_ADNe was created but was empty (no sequence available). No changes were made.\n\n"))

   } else{

   data[["Sequencage_Sanger_ADNe"]] <- tab.int

   cat(crayon::green("\nThe sheet Sequencage_Sanger_ADNe was created as a data frame of", ncol(tab.int), "columns and", nrow(tab.int), "row.", emojifont::emoji("rainbow")  ,"\n\n"))

   }

   return(data)


   } # End of the function

#' @title Extract old eDNA gabarit QNC QPC sequencing info
#'
#' @description
#' Function to extract the QPC QNC info from the qPCR table, as encounter in the eDNA gabarit v3.
#'
#' @details
#' The data object should include a "qPCR_ADNe" table, but not a "QNC_QPC_ADNe" table.
#'
#' @param data List containing important tables.
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso [upload_gabarit_ADNe()] to create the list of tables.
#'
#` @references
#' List references
#' @export

extract_qpc_qnc_ADNe  <- function(data){

  # List the columns we are looking to

  col.to.extract <-  model.eDNA_convert %>% dplyr::filter(Table_ORIGINAL == "qPCR_ADNe",
                                                          Table_NEW == "QNC_QPC_ADNe") %>%
                                            dplyr::pull(Column_ORIGINAL)

  # Stop if not qPCR_ADNe detected
  if(is.null(data[["qPCR_ADNe"]])){
    stop("\nThe sheet qPCR_ADNe is not present in your data.frame, columns cannot be extracted.")
  }

  # Stop if Sequencage_Sanger_ADNe detected
  if(!is.null(data[["QNC_QPC_ADNe"]])){
    stop("\nThe sheet QNC_QPC_ADNe is already present, columns cannot be extracted.")
  }

  tab.int <- data[["qPCR_ADNe"]] %>% dplyr::select(dplyr::all_of(c(col.to.extract, "Type_echantillon_qPCR", "Numero_unique_extrait"#,
                                                                  # paste("QNC", 1:6, sep = "_"), paste("QPC", 1:6, sep = "_")
                                                                 ))) %>%
    dplyr::filter(Type_echantillon_qPCR %in% c("QNC", "QPC"))


  if(all(names(tab.int) !=  c(col.to.extract, "Type_echantillon_qPCR", "Numero_unique_extrait", paste("QNC", 1:6, sep = "_"), paste("QPC", 1:6, sep = "_")))){
    stop(paste("\nThe column names in qPCR_ADNe doesn't perfectly fit the one expected. Changes should be done manually, sorry ..."))
  }


  tab.long.int <- tab.int %>% #dplyr::select(-c(Type_echantillon_qPCR, No_qPCR_ADNe, Numero_unique_extrait)) %>%
                              #dplyr::distinct(.keep_all = T) %>%
                              dplyr::group_by(Plate_ID, Type_echantillon_qPCR) %>%
                              mutate(Numero_repetition_QC_Neg_Pos_ADNe = seq_along(Type_echantillon_qPCR)) %>%
                            dplyr::rename("Type_Neg_Pos_ADNe" = "Type_echantillon_qPCR" ) %>%
                            dplyr::mutate(#Numero_repetition_QC_Neg_Pos_ADNe = sapply(stringr::str_split(Type_Neg_Pos_ADNe, "_"), `[`, 2),
                                          #  Type_Neg_Pos_ADNe = sapply(stringr::str_split(Type_Neg_Pos_ADNe, "_"), `[`, 1),
                                            Modifications_Neg_Pos_ADNe = NA,
                                            Notes_Neg_Pos_ADNe = "Automatically_created_with_extract_qpc_qnc_ADNe_R_function"
                              ) %>%
                              #dplyr::filter(!is.na(Numero_unique_extrait)) %>%
                             # dplyr::left_join(tab.int %>% dplyr::filter(Type_echantillon_qPCR %in% c("QNC", "QPC")) %>%
                             #                   dplyr::select(c(Plate_ID, No_qPCR_ADNe, Numero_unique_extrait )) %>% )
                               dplyr::select(-Numero_unique_extrait) %>% dplyr::ungroup()

  # Message if nothing left

  if( nrow(tab.long.int) == 0){

    cat(crayon::red("\nThe sheet QNC_QPC_ADNe was created but was empty (no QNC QPC). No changes were made.\n\n"))

  } else{

    data[["QNC_QPC_ADNe"]] <- tab.long.int

    cat(crayon::green("\nThe sheet QNC_QPC_ADNe was created as a data frame of", ncol(tab.int), "columns and", nrow(tab.int), "row. PLEASE CHECK THIS NEW TABLE TO CONFIRM EVERYTHING IS OK.", emojifont::emoji("bomb")  ,"\n\n"))

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

    cat("\nPre-correcting column names for", crayon::cyan(x), "whenever possible\n")

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

      cat(ncol(tab.int), "columns were uploaded (", length(model.vec.ORI), "could potentially be automatically renamed)\nNow looking column by column to see if some changes can be done automatically... \n")

      # Check column names one by one

      for(i in 1:length(model.vec.ORI)){ # Loop over each column

        # Do something if you encounter the right name
        if(model.vec.ORI[i] %in% names(tab.int)){

          # Change the name
          names(tab.int)[names(tab.int) ==model.vec.ORI[i]] <- model.vec.NEW[i]

        cat("\nThe column", model.vec.ORI[[i]], "was renamed", model.vec.NEW[[i]] )

        }
      }# END of the LOOP over column names

      data[[x]] <-   tab.int
       cat(crayon::green("\nThe table", x, "is done!\n"))


    } # END of the loop over one table

  }

  cat(crayon::green("\nIt's ALL done", emojifont::emoji("chicken")  ,"\n\n"))

  return(data)

}


