#AKRO CAS Data ----
#Updated 10/6/2022 by C. Tribuzio
#This code will pull the data from AKFIN and clean it up for use in the assessment

# DB direct query setup ----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))

datapath<-paste0(getwd(),"/Data/")

# Get data ----
# This takes a long time, skip this if data have already been queried
CASdat <- sqlQuery(channel_akfin, query = ("
                select year, week_end_date, wed, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code,
                       trip_target_group, trip_target_name, species_group_name, species_name, processor_permit_id,
                       ito_company processor_name, ito_plant processor_plant, ves_akr_adfg, ves_akr_name,	ves_akr_length,
                       retained_or_discarded, weight_posted, gf_harvest_sector, deployment_trip_start_date,	deployment_trip_end_date,
                       deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category, sampling_strata,
                       sampling_strata_name, sampling_strata_selection_rate, management_program_code
                from council.comprehensive_blend_ca
                where year >= 2013
                and (fmp_area = 'GOA' or fmp_area = 'BSAI')")) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  group_by(year, week_end_date, wed, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code,
           trip_target_group, trip_target_name, species_group_name, species_name, processor_permit_id,
           processor_name, processor_plant, ves_akr_adfg, ves_akr_name,	ves_akr_length,
           retained_or_discarded, gf_harvest_sector, deployment_trip_start_date,	deployment_trip_end_date,
           deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category, sampling_strata,
           sampling_strata_name, sampling_strata_selection_rate, management_program_code) %>% 
  summarise(catch_mt = sum(weight_posted))

write_csv(as.data.frame(unique(CASdat$species_name)), paste0(datapath, "species_name_list.csv"))

# this just does some additional formatting, broke it out from SQL to save time if futzing is needed
#CASdat <- read_csv(paste0(getwd(), "/Data/confidential_CAS_all.csv")) read data back in if futzing
CAS_dat <- CASdat %>% 
  mutate(week_end_date = ymd(week_end_date),
         mgmt_area = if_else(fmp_subarea == "BS", "BS",
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

# check to make sure the obs coverage field makes sense
testdat <- CAS_dat %>% select(c(sampling_strata_name, OBS_Coverage))                                     
unique(testdat)

write_csv(CAS_dat, paste0(datapath, "/confidential_CAS_all.csv"))
rm(CASdat)
rm(CAS_dat)

