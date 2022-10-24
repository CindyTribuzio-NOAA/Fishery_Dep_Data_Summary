# Fishery Sample Report ----
# Updated 5/13/2021 by C. Tribuzio

# Q's for AKFIN ----
#1) Can we add cruise and permit to gf catch by fishery?
#2) add week number to norpac age reports

# Setup ----
libs <- c("tidyverse", "reshape2","plyr","lubridate")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

datadir<-paste(getwd(),"/Data/",sep="")
cleandatdir<-paste(getwd(),"/Data/Cleaned/",sep="")
#figdir<-paste(getwd(),"/Output/",AYR,"/Figures/",sep="")

# Get obs deployment info ----
#note that this data set is pre-filtered for sablefish and may not include ALL vessels
catch_dat<-read.csv(paste(datadir,"gfcatchbyfishery_withnewfields_pollock.csv",sep=""),header=T)
catch_dat$WEEK_ENDING_DATE<-ymd(catch_dat$WEEK_ENDING_DATE)
catch_dat<-catch_dat[catch_dat$YEAR>=2016,]
catch_dat$SAMPLING_STRATA_NAME<-droplevels(catch_dat$SAMPLING_STRATA_NAME)
catch_dat<-catch_dat[catch_dat$FMP_AREA!="INSD",]

#change obs strata names to be more easy to use
#rules
#1) full coverage by twl hal or pot
#2) combine trawl EM full and trawl EM partial (only diff is FMP, not important here)
#3) Gear first, then program

catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "Tender Trawl Trip Selection",
                                            "Trawl Tender PC")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                       "Trawl EM Full Coverage Selection",
                                       "Trawl EM")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                       "Trawl EM Parital Coverage Selection",
                                       "Trawl EM")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "Trawl Trip Selection",
                                            "Trawl PC")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "Tender Pot Trip Selection",
                                            "Pot Tender PC")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "Pot Trip Selection",
                                            "Pot PC")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "No Selection",
                                            "Zero Cov")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "Hook-and-Line Trip Selection",
                                            "HAL PC")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "EM Pot Tender Trip Selection",
                                            "Pot Tender EM")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "EM Pot PC",
                                            "Pot EM")
catch_dat$SAMPLING_STRATA_NAME<-str_replace(catch_dat$SAMPLING_STRATA_NAME,
                                            "EM HAL PC",
                                           "HAL EM")

#add A and B season for pollock
#rules
#1) GOA A season goes Jan 20 - aug 25
#2) GOA B season goes Aug 25 - end year
#3) these are the 2021 seasons based on amendment 109, historical data is applied to these rules
#4) EBS A season Jan 20 - June 10
#5) EBS B season goes June 10 - end year
test<-rbind(head(catch_dat),head(catch_dat[catch_dat$FMP_AREA=="GOA",]))
test$SEASON<-NA
test[test$FMP_AREA=="GOA" & test$WED<825,]$SEASON<-"A"
test[test$FMP_AREA=="GOA" & test$WED>=825,]$SEASON<-"B"
test[test$FMP_AREA=="BSAI" & test$WED<610,]$SEASON<-"A"
test[test$FMP_AREA=="BSAI" & test$WED>=610,]$SEASON<-"B"

catch_dat$SEASON<-NA
catch_dat[catch_dat$FMP_AREA=="GOA" & catch_dat$WED<825,]$SEASON<-"A"
catch_dat[catch_dat$FMP_AREA=="GOA" & catch_dat$WED>=825,]$SEASON<-"B"
catch_dat[catch_dat$FMP_AREA=="BSAI" & catch_dat$WED<610,]$SEASON<-"A"
catch_dat[catch_dat$FMP_AREA=="BSAI" & catch_dat$WED>=610,]$SEASON<-"B"

#subset trip specific info
c2<-catch_dat[,c("YEAR","WEEK_ENDING_DATE","SEASON","VESSEL_FEDERAL_PERMIT","VESSEL_ADFG","VESSEL_NAME","VESSEL_LENGTH","TRIP_TARGET_NAME",
                 "PROCESSOR_FEDERAL_PERMIT","HARVEST_SECTOR","DEPLOYMENT_CATEGORY","SAMPLING_STRATA_NAME")]

# Catch Summary ----
# proportion of catch by area and season
catch_props<-ddply(catch_dat,c("YEAR","FMP_AREA","NMFS_AREA","SEASON"),summarize,
                   catch=sum(CATCH_MT))

catch_dat$FMP_SUBAREA <- factor(catch_dat$FMP_SUBAREA, 
                                levels = c("AI","BS","WG","CG","WY","SE"))
ggplot(catch_dat,aes(x=YEAR,y=CATCH_MT,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(FMP_SUBAREA~.)+
  labs(y="Catch (mt)",title="Total Catch by Observer Strata - Pollock")
# take out non-pollock targets then look by area and FMP 
ggplot(catch_dat[catch_dat$TRIP_TARGET_GROUP=="Pollock",],aes(x=YEAR,y=CATCH_MT,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(breaks=scales::pretty_breaks((n=3)))+
  facet_grid(NMFS_AREA~., scales="free_y")+
  labs(y="Catch (mt)",title="Targeted Fishery by Area - Pollock")

# Get biological data ----
biodat<-read.csv(paste(datadir,"norpac_age_report_flattened_pollock.csv",sep=""),header=T, skip=7)

#note differences in column heading styles.....grrrrr
#add week number to match above, it's not going to be a perfect match, but haul offload date is not likely to match week end date.
biodat$Haul.Offload.Date<-dmy(biodat$Haul.Offload.Date)
biodat$WEEK_ENDING_DATE<-ceiling_date(biodat$Haul.Offload.Date,"week")+
  ifelse(weekdays(biodat$Haul.Offload.Date) %in% c("Sunday"),6,-1)
biodat<-biodat[biodat$Year>=2016,]
biodat<-biodat[biodat$FMP.Area!="INSD",]

#biodat$WEEK_NUMBER<-week(biodat$Haul.Offload.Date)

#drop uneeded fields for now
bd<-biodat[,c("T.Table","Year","WEEK_ENDING_DATE","Permit","Vessel","Gear.Description","FMP.Area","FMP.Subarea","NMFS.Area",
              "Type.1.Otolith","Type.3.Sex.Length.Weight","Length..cm.","Weight..kg.","Age")]

# Merge ----
#trip info with biological based on year, week and federal permit
#need to run it separately based on offload sampling or observer sampling

#make matchable permit fields
#when T.Table==ATL_Haul Permit = VESSEL_FEDERAL_PERMIT
#when T.Table==ATL_oFFLOAD Permit = PROCESSOR_FEDERAL_PERMIT

#at-sea obs samples first
OBS<-bd[bd$T.Table=="ATL_HAUL",]
OBS2<-merge(OBS,c2,by.x=c("Year","WEEK_ENDING_DATE","Permit"),
            by.y=c("YEAR","WEEK_ENDING_DATE","VESSEL_FEDERAL_PERMIT"),
            all.x=T)
OBS2<-OBS2[,!(names(OBS2) %in% c("PROCESSOR_FEDERAL_PERMIT"))]

#port obs samples second
PORT<-bd[bd$T.Table=="ATL_OFFLOAD",]
PORT2<-merge(PORT,c2,by.x=c("Year","WEEK_ENDING_DATE","Permit"),
            by.y=c("YEAR","WEEK_ENDING_DATE","PROCESSOR_FEDERAL_PERMIT"),
            all.x=T)
PORT2<-PORT2[,!(names(PORT2) %in% c("VESSEL_FEDERAL_PERMIT"))]
#combine
bio_obs<-rbind(OBS2,PORT2)
bio_obs$FMP.Subarea <- factor(bio_obs$FMP.Subarea, 
                                levels = c("AI","BS","WG","CG","WY","SE"))
write.csv(bio_obs,paste(cleandatdir,"biosampls_w_obs_pollock.csv",sep=""),row.names = F)

#NOTE: still need to track down the ones that don't match up
bio_obs<-bio_obs[!is.na(bio_obs$SAMPLING_STRATA_NAME),]

# Summary Figs ----
# LENGTHS ONLY ----
#coverage group of samples since 2015
ldat<-bio_obs[bio_obs$Type.3.Sex.Length.Weight=="Y",]
ct_l<-ddply(bio_obs,c("Year","SAMPLING_STRATA_NAME"),summarize,
            n_samps=length(Length..cm.))
ggplot(ct_l[ct_l$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="fill", stat="identity")+
  labs(y="Proportion",title="Proportion Lengths by Observer Strata - Pollock")
ggplot(ct_l[ct_l$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  labs(y="n lengths",title="Lengths by Observer Strata - Pollock")

#Lengths by fishing season
ct_ls<-ddply(bio_obs,c("Year","SAMPLING_STRATA_NAME","SEASON","FMP.Area"),summarize,
            n_samps=length(Length..cm.))

ggplot(ct_ls,aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(SEASON~FMP.Area)+
  labs(y="n lengths",title="Lengths by Observer Strata - Pollock")

#coverage group of samples by reg area since 2015
ct_la<-ddply(ldat,c("Year","FMP.Subarea","SAMPLING_STRATA_NAME"),summarize,
            n_samps=length(Length..cm.))
ggplot(ct_la[ct_la$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="fill", stat="identity")+
  facet_grid(FMP.Subarea~.)+
  labs(y="Proportion",title="Proportion Lengths by Observer Strata - Pollock")
ggplot(ct_la[ct_la$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(FMP.Subarea~.)+
  labs(y="n lengths",title="Lengths by Observer Strata - Pollock")

#spatial dist of samples since 2015
sp_bio<-ddply(ldat,c("Year","NMFS.Area","FMP.Subarea","SEASON"),summarize,
              n_samps=length(Length..cm.))
ggplot(sp_bio,aes(x=Year,y=n_samps,fill=as.factor(NMFS.Area)))+
  geom_bar(position="fill", stat="identity")+
  facet_grid(FMP.Subarea~SEASON)+
  scale_y_continuous(breaks=scales::pretty_breaks((n=3)))+
  labs(y="Proportion",title="Proportions by NMFS Area - Pollock")
ggplot(sp_bio[sp_bio$Year>=2015,],aes(x=Year,y=n_samps,fill=as.factor(NMFS.Area)))+
  geom_bar(position="stack", stat="identity")

sp2_bio<-ddply(ldat,c("Year","FMP.Subarea"),summarize,
              n_samps=length(Length..cm.))
ggplot(sp2_bio[sp2_bio$Year>=2015,],aes(x=Year,y=n_samps,fill=FMP.Subarea))+
  geom_bar(position="fill", stat="identity")
ggplot(sp2_bio[sp2_bio$Year>=2015,],aes(x=Year,y=n_samps,fill=FMP.Subarea))+
  geom_bar(position="stack", stat="identity")

# COLLECTED OTOs ONLY ----
#coverage group of samples since 2015
odat<-bio_obs[bio_obs$Type.1.Otolith=="Y",]
ct_o<-ddply(odat,c("Year","SAMPLING_STRATA_NAME"),summarize,
            n_samps=length(Length..cm.))
ggplot(ct_o[ct_o$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="fill", stat="identity")+
  labs(y="Proportion",title="Proportion COLLECTED Otos by Observer Strata - Pollock")
ggplot(ct_o[ct_o$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  labs(y="n otoliths",title="COLLECTED Otos by Observer Strata - Pollock")

#coverage group of samples by reg area since 2015
ct_oa<-ddply(odat,c("Year","FMP.Subarea","SAMPLING_STRATA_NAME"),summarize,
             n_samps=length(Length..cm.))
ggplot(ct_oa[ct_oa$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="fill", stat="identity")+
  facet_grid(FMP.Subarea~.)+
  labs(y="Proportion",title="Proportion COLLECTED Otos by Observer Strata - Sablefish")
ggplot(ct_oa[ct_oa$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(FMP.Subarea~.)+
  labs(y="n otoliths",title="COLLECTED Otos by Observer Strata - Sablefish")

# AGED OTOs ONLY ----
#coverage group of samples since 2015
odat2<-bio_obs[!is.na(bio_obs$Age),]
ct_o_aged<-ddply(odat2,c("Year","SAMPLING_STRATA_NAME"),summarize,
            n_samps=length(Length..cm.))
ggplot(ct_o_aged[ct_o_aged$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="fill", stat="identity")+
  labs(y="Proportion",title="Proportion AGED Otos by Observer Strata - Sablefish")
ggplot(ct_o_aged[ct_o_aged$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  labs(y="n otoliths",title="AGED Otos by Observer Strata - Sablefish")

#coverage group of samples by reg area since 2015
ct_oaaged<-ddply(odat2,c("Year","FMP.Subarea","SAMPLING_STRATA_NAME"),summarize,
             n_samps=length(Length..cm.))
ggplot(ct_oaaged[ct_oaaged$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="fill", stat="identity")+
  facet_grid(FMP.Subarea~.)+
  labs(y="Proportion",title="Proportion COLLECTED Otos by Observer Strata - Sablefish")
ggplot(ct_oaaged[ct_oaaged$Year>=2015,],aes(x=Year,y=n_samps,fill=SAMPLING_STRATA_NAME))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(FMP.Subarea~.)+
  labs(y="n otoliths",title="COLLECTED Otos by Observer Strata - Sablefish")
