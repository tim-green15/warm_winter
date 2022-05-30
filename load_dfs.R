library(tidyverse)
library(lubridate)
library(bigleaf)

# Read in flux data
FLX_FI_Hyy_FLUXNET2015_FULLSET_DD_1996_2020_beta_3 <- read_csv("R:/DATA/ICOS_warm_winter/FLX_FI-Hyy_FLUXNET2015_FULLSET_1996-2020_beta-3/FLX_FI-Hyy_FLUXNET2015_FULLSET_DD_1996-2020_beta-3.csv", 
                                                                    col_types = cols(TIMESTAMP = col_date(format = "%Y%m%d")))

FLX_FI_Ken_FLUXNET2015_FULLSET_DD_2018_2020_beta_3 <- read_csv("R:/DATA/ICOS_warm_winter/FLX_FI-Ken_FLUXNET2015_FULLSET_2018-2020_beta-3/FLX_FI-Ken_FLUXNET2015_FULLSET_DD_2018-2020_beta-3.csv", 
                                                               col_types = cols(TIMESTAMP = col_date(format = "%Y%m%d")))


FLX_FI_Var_FLUXNET2015_FULLSET_DD_2016_2020_beta_3 <- read_csv("R:/DATA/ICOS_warm_winter/FLX_FI-Var_FLUXNET2015_FULLSET_2016-2020_beta-3/FLX_FI-Var_FLUXNET2015_FULLSET_DD_2016-2020_beta-3.csv", 
                                                               col_types = cols(TIMESTAMP = col_date(format = "%Y%m%d")))

FLX_SE_Htm_FLUXNET2015_FULLSET_DD_2015_2020_beta_3 <- read_csv("R:/DATA/ICOS_warm_winter/FLX_SE-Htm_FLUXNET2015_FULLSET_2015-2020_beta-3/FLX_SE-Htm_FLUXNET2015_FULLSET_DD_2015-2020_beta-3.csv", 
                                                                    col_types = cols(TIMESTAMP = col_date(format = "%Y%m%d")))

FLX_SE_Nor_FLUXNET2015_FULLSET_DD_2014_2020_beta_3 <- read_csv("R:/DATA/ICOS_warm_winter/FLX_SE-Nor_FLUXNET2015_FULLSET_2014-2020_beta-3/FLX_SE-Nor_FLUXNET2015_FULLSET_DD_2014-2020_beta-3.csv", 
                                                                    col_types = cols(TIMESTAMP = col_date(format = "%Y%m%d")))
FLX_SE_Svb_FLUXNET2015_FULLSET_DD_2014_2020_beta_3 <- read_csv("R:/DATA/ICOS_warm_winter/FLX_SE-Svb_FLUXNET2015_FULLSET_2014-2020_beta-3/FLX_SE-Svb_FLUXNET2015_FULLSET_DD_2014-2020_beta-3.csv", 
                                                                    col_types = cols(TIMESTAMP = col_date(format = "%Y%m%d")))

##########
# Hyytiala
###########



### select desired variables
FI_Hyy <- FLX_FI_Hyy_FLUXNET2015_FULLSET_DD_1996_2020_beta_3 %>%
  select(TIMESTAMP, TA_F, TA_F_QC, SW_IN_F, SW_IN_F_QC, VPD_F, VPD_F_QC, NEE_VUT_REF, NEE_VUT_REF_QC,
         GPP_NT_VUT_REF, RECO_NT_VUT_REF) 


### filter years, months, add column of site name, filter by NEE QC
FI_Hyy_18_20_spring <- FI_Hyy %>%
  filter(TIMESTAMP >= "2018-01-01") %>%
  mutate(month = month(TIMESTAMP)) %>%
  filter(month <= 6) %>%
  mutate(Site = "FI-Hyy", Species = "pine", Lat = "south") %>%
  filter(NEE_VUT_REF_QC >= 0.9)


#############
# Kenttarova
#############

### select desired variables
FI_Ken <- FLX_FI_Ken_FLUXNET2015_FULLSET_DD_2018_2020_beta_3 %>%
  select(TIMESTAMP, TA_F, TA_F_QC, SW_IN_F, SW_IN_F_QC, VPD_F, VPD_F_QC, NEE_VUT_REF, NEE_VUT_REF_QC,
         GPP_NT_VUT_REF, RECO_NT_VUT_REF) 


### filter years, months, add column of site name, filter by NEE QC
FI_Ken_18_20_spring <- FI_Ken %>%
  filter(TIMESTAMP >= "2018-01-01") %>%
  mutate(month = month(TIMESTAMP)) %>%
  filter(month <= 6) %>%
  mutate(Site = "FI-Ken", Species = "spruce", Lat = "north") %>%
  filter(NEE_VUT_REF_QC >= 0.9)

#############
# Varrio
#############

### select desired variables
FI_Var <- FLX_FI_Var_FLUXNET2015_FULLSET_DD_2016_2020_beta_3 %>%
  select(TIMESTAMP, TA_F, TA_F_QC, SW_IN_F, SW_IN_F_QC, VPD_F, VPD_F_QC, NEE_VUT_REF, NEE_VUT_REF_QC,
         GPP_NT_VUT_REF, RECO_NT_VUT_REF) 


### filter years, months, add column of site name, filter by NEE QC
FI_Var_18_20_spring <- FI_Var %>%
  filter(TIMESTAMP >= "2018-01-01") %>%
  mutate(month = month(TIMESTAMP)) %>%
  filter(month <= 6) %>%
  mutate(Site = "FI-Var", Species = "pine", Lat = "north") %>%
  filter(NEE_VUT_REF_QC >= 0.9)

#############
# Hyltemossa
#############

### select desired variables
SE_Htm <- FLX_SE_Htm_FLUXNET2015_FULLSET_DD_2015_2020_beta_3 %>%
  select(TIMESTAMP, TA_F, TA_F_QC, SW_IN_F, SW_IN_F_QC, VPD_F, VPD_F_QC, NEE_VUT_REF, NEE_VUT_REF_QC,
         GPP_NT_VUT_REF, RECO_NT_VUT_REF) 


### filter years, months, add column of site name, filter by NEE QC
SE_Htm_18_20_spring <- SE_Htm %>%
  filter(TIMESTAMP >= "2018-01-01") %>%
  mutate(month = month(TIMESTAMP)) %>%
  filter(month <= 6) %>%
  mutate(Site = "SE-Htm", Species = "spruce", Lat = "south") %>%
  filter(NEE_VUT_REF_QC >= 0.9)

#############
# Norunda
#############

### select desired variables
SE_Nor <- FLX_SE_Nor_FLUXNET2015_FULLSET_DD_2014_2020_beta_3 %>%
  select(TIMESTAMP, TA_F, TA_F_QC, SW_IN_F, SW_IN_F_QC, VPD_F, VPD_F_QC, NEE_VUT_REF, NEE_VUT_REF_QC,
         GPP_NT_VUT_REF, RECO_NT_VUT_REF) 


### filter years, months, add column of site name, filter by NEE QC
SE_Nor_18_20_spring <- SE_Nor %>%
  filter(TIMESTAMP >= "2018-01-01") %>%
  mutate(month = month(TIMESTAMP)) %>%
  filter(month <= 6) %>%
  mutate(Site = "SE-Nor", Species = "mix", Lat = "south") %>%
  filter(NEE_VUT_REF_QC >= 0.9)

#############
# Svartberget
#############

### select desired variables
SE_Svb <- FLX_SE_Svb_FLUXNET2015_FULLSET_DD_2014_2020_beta_3 %>%
  select(TIMESTAMP, TA_F, TA_F_QC, SW_IN_F, SW_IN_F_QC, VPD_F, VPD_F_QC, NEE_VUT_REF, NEE_VUT_REF_QC,
         GPP_NT_VUT_REF, RECO_NT_VUT_REF) 


### filter years, months, add column of site name, filter by NEE QC
SE_Svb_18_20_spring <- SE_Svb %>%
  filter(TIMESTAMP >= "2018-01-01") %>%
  mutate(month = month(TIMESTAMP)) %>%
  filter(month <= 6) %>%
  mutate(Site = "SE-Svb", Species = "mix", Lat = "central") %>%
  filter(NEE_VUT_REF_QC >= 0.9)


####################################################################
#####################################################################
######################################################################

# create overall spring data frame

spring_all <- rbind(FI_Hyy_18_20_spring, FI_Ken_18_20_spring, FI_Var_18_20_spring, SE_Htm_18_20_spring, SE_Svb_18_20_spring,
                         SE_Nor_18_20_spring)

# covert character columns to factor and convert fluxes to gC/m2/day
spring_all <- spring_all %>%
  mutate_if(is.character, as.factor) %>%
  mutate(GPP_NT_VUT_REF = umolCO2.to.gC(GPP_NT_VUT_REF), NEE_VUT_REF = umolCO2.to.gC(NEE_VUT_REF),
         RECO_NT_VUT_REF = umolCO2.to.gC(RECO_NT_VUT_REF))


