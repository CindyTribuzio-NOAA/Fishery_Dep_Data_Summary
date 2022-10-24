#NORPAC biological sample Data ----
#Updated 10/20/2022 by C. Tribuzio
#This code will pull the data from AKFIN and clean it up for use in the analysis

# DB direct query setup ----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))

datapath<-paste0(getwd(),"/Data/")

# Get data ----
# make a species list, need to be able to connect this to CAS data
#NORPAC_species <- sqlQuery(channel_akfin, query = ("
#                select *
#                from NORPAC.debriefed_spcomp")) %>%
#  clean_names() %>% 
#  group_by(species_name, species) %>% 
#  summarise(tot_samps = sum(sample_number))
#write_csv(NORPAC_species, paste0(datapath, "/NORPAC_species_codes.csv"))

# pull in all biological sample data
NORPACdat <- sqlQuery(channel_akfin, query = ("
                select join_key, species_key, cruise, permit, vessel, gear, nmfs_area, specimen_number, Type_1_otolith, type_3_sex_length_weight, year,
                       gear_description, akr_gear, fmp_gear, fmp_area, fmp_subarea, species_name, akr_species_codes, deployment_trip_pk,
                       sampling_strata, sampling_strata_name, sampling_strata_deployment_category, sampling_strata_selection_rate, monitoring_status
                from NORPAC.debriefed_age_flat_mv
                where YEAR >= 2013 and
                NMFS_AREA <= 650")) %>%
  clean_names() %>% 
  as.data.frame()

NORPACdat <- NORPACdat %>% 
  filter(nmfs_area != 649) %>% 
  mutate(mgmt_area = if_else(fmp_subarea == "BS", "BS",
                             if_else(fmp_subarea == "AI", "AI",
                                     if_else(fmp_subarea == "WG", "WGOA",
                                             if_else(fmp_subarea == "CG", "CGOA", "EGOA")))),
         OBS_Coverage = if_else(sampling_strata_name == "No Selection", "None",
                                if_else(sampling_strata_name == "Full Selection" |
                                          sampling_strata_name == "Trawl EM Full Coverage Selection", "Full", 
                                        if_else(sampling_strata_name == "EM Hook-and-Line Trip Selection" |
                                                  sampling_strata_name == "EM Pot Trip Selection" |
                                                  sampling_strata_name == "EM Pot Tender Trip Selection" |
                                                  sampling_strata_name == "Trawl EM Partial Coverage Selection", "EM Partial", 
                                                if_else(sampling_strata_name == "Trawl EM Full Coverage Selection", "EM Full", "Partial")))),
         observed = if_else(monitoring_status == "FIXED_GEAR_EM", "Catch Only",
                            if_else(monitoring_status == "NO_MONITORING", "None","Catch/Biologicals")))
write_csv(NORPACdat, paste0(datapath, "confidential_NORPAC_flattened_sampledata.csv"))


rm(NORPACdat)
