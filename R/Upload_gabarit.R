#' @title Upload Excel template
#'
#' @description
#' Function to upload an Excel file based on the predefined template
#'
#' @details
#' Optional if you want to add more detais.
#'
#' @param path Path to the excel spreadsheet
#' @param skip N row to skip
#'
#' @examples
#' # provide some examples of how to use your function
#' hello()
#'
#' @seealso List relevant other functions [littleforecast()].
#'
#` @references
#' List references
#' @export

upload_gabarit_ADN <- function(path, skip=1){
  # Etape 1 - verifier que les noms suivent le gabarit

  sheet.DNA <- readxl::excel_sheets(path = path)

  # Etape 2 - loader chacune des pages, et les mettre dans une liste
  # On pourrait imprimer certaines statistiques

  print(sheet.DNA)

  # Etape 3 - retourner la liste - c'est à partir d'elle qu'on va travailler

  return()

}

