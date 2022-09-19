
# Upload the library

library(BDLG.gestion)


?upload_gabarit_ADN


# Load an Excel gabarit ---------------------------------------------------

gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/20220901_MOBELS.xlsx")


gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/20220901_MOBELS.xlsx",
                                 specimen = "Specimens",
                                 tissu = "Tissus",
                                 extrait = "Extraits_ADN_ARN",
                                 analyse_ext = "Analyses_externes",
                                 sexage = "05_qPCR",
                                 dloop = NULL,
                                 skip = 0)

# Step 1 - Upload Excel file in R

gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/2021_CHLAMYS.xlsx")


gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/2021_CHLAMYS.xlsx",
                                 specimen = "Specimens",
                                 tissu = "Tissus",
                                 extraitADN = "Extraits_ADN_ARN",
                                 analyse_ext = "Analyses_externes",
                                 sexage = NULL,
                                 dloop = NULL,
                                 skip = 0)


str(gabarit.ls)
names(gabarit.ls)

names(gabarit.ls)

View(head(gabarit.ls$Extrait))

# Step 2 - Check column names and order - and do the corrections
gabarit.ls2 <- check_column_name(gabarit.ls)



# Checks that will need hand correction someimes
# Just need to run it, and check if something must be done
check_column_values_ID(gabarit.ls2)



gabarit.ls3 <- correct_column_values_factor(gabarit.ls2)


data <- gabarit.ls2

dplyr::select(gabarit.ls$Extrait, "Numero_unique_specimen")


 names(gabarit.ls$Extrait)



col <- readxl::read_excel("inst/BD_format.xlsx")
col


readr::read_csv("inst/BD_columns_format.csv")
