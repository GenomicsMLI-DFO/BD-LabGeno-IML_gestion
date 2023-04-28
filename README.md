# BDLG.gestion
Package pour faciliter l'utilisation et la gestion de la base de données en génomique

__Main author:__  Audrey Bourret  
__Affiliation:__  Fisheries and Oceans Canada (DFO)   
__Group:__        Laboratory of genomics   
__Location:__     Maurice Lamontagne Institute  
__Affiliated publication:__  
__Contact:__      e-mail: audrey.bourret@dfo-mpo.gc.ca

- [Installation du package](#installation-du-package)
- [Comment utiliser le package](#comment-utiliser-le-package)

## Installation du package

Pour installer le package directement à partir de R:

```{r}
library(remotes)
remotes::install_github("GenomicsMLI-DFO/BD-LabGeno-IML_gestion")
```

For windows user, it is possible that you get into problems. You can try to set this parameter before the installation.

```{r}
# Change the behaviours of remotes
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
```

## Comment utiliser le package 

Voir le dossier [BD-LabGeno-IML_correction_template](https://github.com/GenomicsMLI-DFO/BD-LabGeno-IML_correction_template) pour un exemple.


