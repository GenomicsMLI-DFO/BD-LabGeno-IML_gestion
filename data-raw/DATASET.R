## code to prepare the dataset using usethis::use_data

model.ordre <- readxl::read_excel("inst/BD_format.xlsx", sheet = "Ordre")
model.ID   <-  readxl::read_excel("inst/BD_format.xlsx", sheet = "ID")
model.list <-  readxl::read_excel("inst/BD_format.xlsx", sheet = "Valeurs")
model.date <- readxl::read_excel("inst/BD_format.xlsx", sheet = "Date")
model.other <- readxl::read_excel("inst/BD_format.xlsx", sheet = "Others")
model.eDNA_convert <- readxl::read_excel("inst/BD_format.xlsx", sheet = "Modif_ADNe_column")

# Save the cleaned data in the required R package location


usethis::use_data(model.ordre,
                  model.ID,
                  model.list,
                  model.date,
                  model.other,
                  overwrite = T)
