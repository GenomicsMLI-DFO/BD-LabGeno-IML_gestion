
# Upload the library

library(BDLG.gestion)



# Load an Excel gabarit ---------------------------------------------------

gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/20220901_MOBELS.xlsx")


gabarit.ls <- upload_gabarit_ADN(path = "inst/extdata/20220901_MOBELS.xlsx",
                                 Specimen = "Specimens",
                                 Tissu = "Tissus",
                                 Extrait = "Extraits_ADN_ARN",
                                 Analyse_Ext = "Analyses_externes",
                                 DLoop = "D-Loop",
                                 skip = 0)

str(gabarit.ls)


names(gabarit.ls)

head(gabarit.ls$Specimen)
