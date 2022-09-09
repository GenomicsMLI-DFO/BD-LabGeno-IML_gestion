
# Upload the library

library(BDLG.gestion)



# Load an Excel gabarit ---------------------------------------------------

gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/20220901_MOBELS.xlsx")


gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/20220901_MOBELS.xlsx",
                                 specimen = "Specimens",
                                 tissu = "Tissus",
                                 extrait = "Extraits_ADN_ARN",
                                 analyse_ext = "Analyses_externes",
                                 sexage = "05_qPCR",
                                 DLoop = "D-Loop",
                                 skip = 0)


gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/2021_CHLAMYS.xlsx")


gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/2021_CHLAMYS.xlsx",
                                 specimen = "Specimens",
                                 tissu = "Tissus",
                                 extrait = "Extraits_ADN_ARN",
                                 analyse_ext = "Analyses_externes",
                                 sexage = NULL,
                                 dloop = NULL,
                                 skip = 0)


str(gabarit.ls)


names(gabarit.ls)

head(gabarit.ls$Specimen)
