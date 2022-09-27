

# Library -----------------------------------------------------------------

# Upload the library (should be installed first)

library(BDLG.gestion)
library(tidyverse)

# File to check -----------------------------------------------------------

path.excel <- "2022_CHLAMYS.xlsx"
path.excel

file.exists(path.excel)

# Step 1. Load an Excel gabarit ---------------------------------------------------

?upload_gabarit_ADN

gabarit.ls <- upload_gabarit_ADN(path = path.excel)

# Change the names of the sheet following what aws observed

gabarit.ls <- upload_gabarit_ADN(path = path.excel,
                                 groupe = "Groupe",
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

data <- gabarit.ls.2


# Step 2 Correct column names and order ------------------------------------------

#Check column names and order - and do the corrections

gabarit.ls.2 <- correct_column_name(gabarit.ls)

names(gabarit.ls.2)

export_access_xlsx(data = gabarit.ls.2,
                   path = ".",
                   prefix = "Chlamys_v1_20220926")


gabarit.ls.2 <- upload_gabarit_ADN(path = "Chlamys_v1_20220926.xlsx",
                                   sexage = NULL,
                                   dloop = NULL,
                                   skip = 0)

# Step 3 Check pre-defined column format ----------------------------------

# Checks that will need hand correction sometimes
# Just need to run it, and check if something must be done
check_column_values_ID(gabarit.ls.2)


# Step 4 Correct factors (predefined values) ------------------------------

gabarit.ls.3 <- correct_column_values_factor(gabarit.ls.2)


# Step 5 Correct dates ----------------------------------------------------

gabarit.ls.4  <- correct_column_values_date(gabarit.ls.3)

# Step 6 Correct columns that should be numbers

gabarit.ls.5  <- correct_column_values_numeric(gabarit.ls.4)


# Step 7  Correct well format -------------------------------------------------------

gabarit.ls.6 <- correct_column_values_well(gabarit.ls.5)

# Step 8 Check all other columns  ------------------------------------------

gabarit.ls.7 <- correct_column_values_others(gabarit.ls.6)




check_relation(gabarit.ls.2)


# Export the results ------------------------------------------------------

# In csv version

export_access_csv(data = gabarit.ls.2,
                  path = ".",
                  prefix = "Chlamys_test")

# In xlsx version

export_access_xlsx(data = gabarit.ls.2,
                   path = ".",
                   prefix = "Chlamys_v1_20220926")



sink(file = "console_output.txt", append = TRUE)

gabarit.ls.4  <- correct_column_values_well(gabarit.ls.2)

sink(file = NULL)


xlsx::write.xlsx()

data <- gabarit.ls.2

