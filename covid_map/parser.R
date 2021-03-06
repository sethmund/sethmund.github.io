library(tidyverse)
library(tools)
library(readxl)

dat <- read.csv("C:/Users/Seth/Desktop/COVID_data/confirmed.csv")

states <- dat %>% 
  pivot_longer(5:ncol(.), names_to = "Date", values_to = "Cases") %>% 
  mutate(Date = str_remove(Date,"X"),
         Date = as.Date(Date, "%Y.%m.%d")) %>% 
  group_by(stateAbbr,stateFIPS) %>% 
  filter(Cases == max(Cases)) %>% 
  distinct(stateFIPS,Cases) %>% 
  ungroup() %>% 
  transmute(id = str_pad(as.character(stateFIPS), 2, pad = "0"),
            id = paste0(id,"000"),
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

fips <- read.csv("fips.csv")%>% mutate(id = str_pad(as.character(id), 5, pad = "0")) %>% filter(grepl(",",name))

cases <- fips %>% 
      left_join(counties) %>% 
  mutate(cases = case_when(is.na(cases) ~ 0,
                          TRUE ~ as.numeric(cases))) %>% rbind(states)

#==============================================================================

write.csv(cases, "data.csv",row.names=FALSE)
