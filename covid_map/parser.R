library(tidyverse)
library(tools)
library(readxl)

dat <- read.csv("https://raw.githubusercontent.com/sethmund/COVID_data/master/confirmed.csv")

states <- dat %>% 
  pivot_longer(5:ncol(.), names_to = "Date", values_to = "Cases") %>% 
  mutate(Date = str_remove(Date,"X"),
         Date = as.Date(Date, "%Y.%m.%d")) %>% 
  group_by(stateAbbr,stateFIPS) %>% 
  filter(Cases == max(Cases)) %>% 
  distinct(stateFIPS,Cases) %>% 
  ungroup() %>% 
  transmute(id = str_pad(as.character(stateFIPS), 2, pad = "0"),
            name = state.name[match(stateAbbr, state.abb)],
         cases = Cases)
 
counties <-  dat %>% 
  pivot_longer(5:ncol(.), names_to = "Date", values_to = "Cases") %>% 
  mutate(Date = str_remove(Date,"X"),
         Date = as.Date(Date, "%Y.%m.%d")) %>% 
  group_by(countyFIPS) %>% 
  filter(Cases == max(Cases), county != "") %>% 
  ungroup() %>% 
  distinct(countyFIPS, county,stateAbbr,Cases) %>% 
  select(id = countyFIPS,
         county,
         stateAbbr,
         cases = Cases) %>% 
  transmute(id = str_pad(as.character(id), 5, pad = "0"),
            cases)

#FIPS=====================================================================

county_fips <- read_xls("fips_codes_website.xls") %>% select(code=1,stfips=2,cfips=3,name=6,type=7) %>% 
  filter(type %in% c("Borough","County","Parish","CDP","TDSA")) %>% 
  transmute(id = paste0(stfips,cfips),
            name = paste0(name, ", ",code))

cases <- county_fips %>% 
      left_join(counties) %>% 
  mutate(cases = case_when(is.na(cases) ~ 0,
                          TRUE ~ as.numeric(cases))) %>% rbind(states)

#==============================================================================

write.csv(cases, "data.csv",row.names=FALSE)

