#Catch data summary ----
#Updated 10/6/2022 by C. Tribuzio
#

# Setup ----
datapath<-paste0(getwd(),"/Data/")
outpath<-paste0(getwd(),"/Output/")

# Bring in CAS data ----
CASdat <- read_csv(paste0(datapath, "confidential_CAS_all.csv"))


