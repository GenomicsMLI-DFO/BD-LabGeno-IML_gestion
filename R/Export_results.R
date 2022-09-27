#' @title Upload Excel template
#'
#' @description
#' Function to export the list into a csv
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data Path to the excel spreadsheet
#' @param path Path to the excel spreadsheet
#' @param prefix Prefi of the new file name
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso List relevant other functions [littleforecast()].
#'
#` @references
#' List references
#' @export

export_access_csv <- function(data,
                          path = ".",
                          prefix = "Export"){


  for(x in names(data)){

    tab.int <- data[[x]]
    new.file <- file.path(path, paste0(prefix, "_", x, ".csv"))

    cat("\nExporting", crayon::cyan(x), "to", new.file, "\n")

    readr::write_csv(tab.int, file = new.file, na = "")


    }

  cat(crayon::green("\nExportation is OVER!!"),  emojifont::emoji("tropical_drink")  ,"\n\n")

}

#' @title Upload Excel template
#'
#' @description
#' Function to export the list into a csv
#'
#' @details
#' NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
#'
#' @param data Path to the excel spreadsheet
#' @param path Path to the excel spreadsheet
#' @param prefix Prefi of the new file name
#'
#' @examples
#' # provide some examples of how to use your function
#'
#'
#' @seealso List relevant other functions [littleforecast()].
#'
#` @references
#' List references
#' @export

export_access_xlsx <- function(data,
                              path = ".",
                              prefix = "Export"){

  new.file <- file.path(path, paste0(prefix, ".xlsx"))
  for(x in 1:length(names(data))){
    x.id <- names(data)[x]

    tab.int <- data[[x.id]] %>% as.data.frame()


    cat("\nExporting", crayon::cyan(x.id), "to", new.file, "\n")

    if(x == 1) {

    xlsx::write.xlsx(tab.int, file = new.file, col.names = TRUE, row.names = FALSE,
                      sheetName = x.id, append = F, showNA = F)

    }

    if(x > 1) {

      xlsx::write.xlsx(tab.int, file = new.file, col.names = TRUE, row.names = FALSE,
                       sheetName = x.id, append = T, showNA = F)

    }


  }

  cat(crayon::green("\nExportation is OVER!!"),  emojifont::emoji("tropical_drink")  ,"\n\n")

}
