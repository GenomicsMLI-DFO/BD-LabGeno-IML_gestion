#' @title Remove all special caracterd
#'
#' @description
#' Function to check that column name and order follow the predefined template
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param x a string
#'
#' @examples
#' # provide some examples of how to use your function
#'
remove_all_special <- function(x) {

  ## Mot ingérable dans ACCESS
  #x <- "  îlés-(de)-la-Madeleine!?,$@#~!%^&*(){}+:\"<>?,./;'[]="
  #x

  # Enlever les espaces au début ou à la fin
  x <- stringr::str_trim(x)
  x

  # Remplacer tout les accents
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x

  # Changer les espaces et tiret par des _
  x <- stringr::str_replace_all(x, paste("[:space:]","-", sep = "|"), "_" )
  x

  # Enlever la plupart des problèmes sauf _
  x <- stringr::str_remove_all(x,"[[:punct:]&&[^_]]")
  x

  # Enlever tout les autres caractères problématiques
  x <- stringr::str_remove_all(x, paste("\\$", "[~]", "\\^", "\\+", "\\=", "<", ">", sep = "|"))

  return(x)

}
