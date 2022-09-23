

# Library -----------------------------------------------------------------

# Upload the library (should be installed first)

library(BDLG.gestion)
library(tidyverse)

# File to check -----------------------------------------------------------

path.excel <- "inst/extdata/2021_CHLAMYS.xlsx"
path.excel

file.exists(path.excel)

# Step 1. Load an Excel gabarit ---------------------------------------------------

?upload_gabarit_ADN

gabarit.ls <- upload_gabarit_ADN(path = path.excel)

# Change the names of the sheet following what aws observed

gabarit.ls <- upload_gabarit_ADN(path = path.excel,
                                 specimen = "Specimens",
                                 tissu = "Tissus",
                                 extraitADN = "Extraits_ADN_ARN",
                                 analyse_ext = "Analyses_externes",
                                 sexage = NULL,
                                 dloop = NULL,
                                 skip = 0)

# Check that the dimension make sense

str(gabarit.ls)
names(gabarit.ls)

View(head(gabarit.ls$Extrait))


# Step 2 Correct column names and order ------------------------------------------

#Check column names and order - and do the corrections

gabarit.ls.2 <- correct_column_name(gabarit.ls)



# Step 3 Check pre-defined column format ----------------------------------

# Checks that will need hand correction sometimes
# Just need to run it, and check if something must be done
check_column_values_ID(gabarit.ls.2)



# Step 4 Correct factors --------------------------------------------------

gabarit.ls.3 <- correct_column_values_factor(gabarit.ls.2)


# Step 5 Correct dates ----------------------------------------------------

gabarit.ls.4  <- correct_column_values_date(gabarit.ls.3)

# Check also missing values

gabarit.ls.4  <- correct_column_values_numeric(gabarit.ls.2)



# Export the results ------------------------------------------------------

export_access_csv(data = gabarit.ls.2,
                  path = ".",
                  prefix = "Chlamys_test")


export_access_xlsx(data = gabarit.ls.2,
                   path = ".",
                   prefix = "Chlamys_test")



sink(file = "console_output.txt", append = TRUE)

gabarit.ls.4  <- correct_column_values_well(gabarit.ls.2)

sink(file = NULL)


xlsx::write.xlsx()

data <- gabarit.ls.2

