#' @title Remove all special characters
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
#' @export

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



#' @title Correct lat - long
#'
#' @description
#' Function to correct some weird format of latitude and longitude
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param x a string
#'
#' @examples
#' # In the Tel format
#' x <- 4753.1
#' correct_latlon(x, format = "NGSL")
#'
#' # In the dms format
#' x <- "47° 22' 38,121 N"
#' correct_latlon(x, format = "dms")
#'
#' @export

correct_latlon <- function(x, format = c("NGSL", "dms")){

 if(format == "NGSL") {

  x.int.1 <- as.numeric(stringr::str_sub(x, 1,2))
  x.int.2 <- as.numeric(stringr::str_sub(x, 3)) / 60

  x.corrected <- x.int.1 + x.int.2

  return(x.corrected)
 }

 if(format == "dms"){

  x.int.1  <- sapply(stringr::str_split(x, pattern = "°|'|\""), `[`, 1) %>% stringr::str_trim() %>% stringr::str_replace("[.]", ",")  %>% as.numeric()
  x.int.2  <- sapply(stringr::str_split(x, pattern = "°|'|\""), `[`, 2) %>% stringr::str_trim() %>% stringr::str_replace("[.]", ",")  %>% as.numeric()
  x.int.3  <- sapply(stringr::str_split(x, pattern = "°|'|\""), `[`, 3) %>% stringr::str_remove("N|S|E|W|O") %>%  stringr::str_trim() %>% stringr::str_replace(",", ".")  %>% as.numeric()

  x.corrected <- x.int.1 + x.int.2/60 + x.int.3/3600

  return(x.corrected)
 }

}



