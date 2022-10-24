# Fishery Data Report ----
# Updated 2/12/2021 by C. Tribuzio

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", "grid", 
          "flextable", "officer", "lubridate", "RODBC", "patchwork","bookdown","knitr", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Create Directories ----
dir.create(paste0(getwd(),"/Data/"), showWarnings = T)
dir.create(paste0(getwd(),"/Output/"), showWarnings = T)
dir.create(paste0(getwd(),"/Documents/"), showWarnings = T)

# Get Data ----
# Query CAS data
# Skip if data have already been queried, it's a LOOOOOOONG query
# with loooonnnnggg and large files
# may be better run on VM
source(paste(getwd(),"/Code/Get_CAS_data.R",sep=""))

spec_list <- read_csv(paste0(getwd(), "/Data/species_name_list.csv"))

spec_name <- "sablefish (blackcod)" #insert your desired species name or list of names here

# Query NORPAC biological data ----
source(paste(getwd(),"/Code/Get_NORPAC_data.R",sep=""))




