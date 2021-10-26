########## <01 Analysis for PMTCT_HEI#############

library(tidyverse)

setwd("C:/Working Folder/Downloads/MER_Structured_Datasets_PSNU_IM_FY18-21_20201113_v1_1")

df<-read_tsv("MER_Structured_Datasets_PSNU_IM_FY18-21_20201113_v1_1.txt")

unique(df$indicator)

# Create indicator - disaggregate key table
df1<-df %>% 
  filter(indicator %in% c("TX_PVLS", "TX_CURR", "TX_NEW", "TX_NET_NEW", "PMTCT_EID_POS",
                          "PMTCT_HEI_POS_2MO", "PMTCT_HEI_POS", "PMTCT_HEI_POS_ART"))
key<- df1 %>% 
  distinct(indicator, standardizeddisaggregate)


#Create dataset with indicators and standardizeddisaggregate
df2<-df %>% 
  filter(indicator %in% c("TX_PVLS", "TX_CURR", "TX_NEW", "TX_NET_NEW", "PMTCT_EID_POS",
                          "PMTCT_HEI_POS_2MO", "PMTCT_HEI_POS", "PMTCT_HEI_POS_ART")) %>% 
  filter(standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Total Denominator",
                                         "Total Numerator", "Age/Sex/Indication/HIVStatus", "Age/HIVStatus",
                                         "Age/HIVStatus/ARTStatus")) 

#drop indicators and create new time variable  
df3<-df2 %>% 
  gather(period,val,targets:cumulative) %>% 
  mutate(nperiod = paste("FY", fiscal_year, period, sep ="")) %>% 
  select(- c("source_name", "otherdisaggregate_sub", "modality",
             "fiscal_year", "period", "statustb", "statuscx")) 

#create sex variable dataset that is wide by new time variable
df4<- df3 %>% 
  group_by(operatingunit,countryname, snu1, psnu, snuprioritization, primepartner, fundingagency, mech_code,
           mech_name, indicator, numeratordenom, disaggregate, standardizeddisaggregate, ageasentered,
           trendsfine,sex, otherdisaggregate,nperiod) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE) %>%
  #ungroup() %>% 
  spread(nperiod, val)

#create dataset without sex and wide by new time variable
df5<- df3 %>% 
  group_by(operatingunit,countryname, snu1, psnu, snuprioritization, primepartner, fundingagency, mech_code,
           mech_name, indicator, numeratordenom, disaggregate, standardizeddisaggregate, ageasentered,
           trendsfine, otherdisaggregate,nperiod) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE) %>%
  #ungroup() %>% 
  spread(nperiod, val)

#Create output datasets
write_tsv(df5, "Indicatorfilenosex.txt", na=" ")
write_tsv(df4, "Indicatorfile.txt", na=" ")
write_tsv(key, "standarddizeddisaggkey.txt", na=" ")

rm(list=ls())

