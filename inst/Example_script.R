
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


dplyr::select(gabarit.ls$Extrait, "Numero_unique_specimen")


 names(gabarit.ls$Extrait)



col <- readxl::read_excel("inst/BD_format.xlsx")
col


readr::read_csv("inst/BD_columns_format.csv")
