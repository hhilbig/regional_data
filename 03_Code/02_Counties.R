## 

wd <- 'C:/Users/Hanno/Dropbox/Harvard/Projects/Github/regional_data/'
setwd(wd)

##

rm(list = ls())

##

library(dplyr)
library(xlsx)
library(pbapply)
library(hannostuff2)
library(stringr)
library(readr)
library(readxl)
library(openxlsx)
library(reshape2)

#### Unemmployment ####

unem_county <- read_delim('01_Raw_Data/Counties/kreis_unem.csv', skip = 3,
                          delim = ';', )

## Rename 

vnames <- c('year', 'ags', 'name', 'unem_tot',
            'unem_foreign', 'unem_handicapped',
            'unem_age_15_20', 'unem_age_15_25',
            'unem_age_55_65', 'unem_longterm',
            'unem_rate_dependent', 'unem_rate_tot',
            'unem_rate_male', 'unem_rate_female',
            'unem_rate_foreign', 'unem_rate_age_15_25')

## Remove first few rows, assign column names

unem_county <- unem_county %>% slice(3:n())
colnames(unem_county) <- vnames

## drop everyhting but Kreise, deal with decimal char

unem_county <- unem_county %>% 
  mutate_at(vars(c('unem_rate_dependent', 'unem_rate_tot',
                   'unem_rate_male', 'unem_rate_female',
                   'unem_rate_foreign', 'unem_rate_age_15_25')),
            funs(str_replace_all(., pattern = ',', replacement = '.'))) %>%
  mutate_at(vars(vnames[-c(1:3)]),
            as.numeric) %>%
  filter(nchar(ags) == 5) 

## Split this into yearly data frames

list_df <- lapply(unique(unem_county$year), function(y) {
  unem_county %>% filter(year == y)
}) 

names(list_df) <- paste0('year_', unique(unem_county$year))

#### Religion ####

relig11 <- read_delim('01_Raw_Data/Counties/kreis_religion_census_2011.csv',
                      delim = ';', skip = 2)

vnames2 <- c('ags', 'name', 'relig', 'tot', 'male', 
             'female', 'native', 'foreign')
colnames(relig11) <- vnames2  

## Clean

relig11 <- relig11 %>% slice(7:n()) %>%
  filter(!is.na(name)) %>%
  filter(nchar(ags) == 5) %>%
  mutate_at(vars(vnames2[4:8]), as.numeric) %>%
  dplyr::select(-one_of(c('name', 'native', 'foreign', 'male', 'female'))) %>%
  mutate(relig = factor(relig))

## New factor levels

levels(relig11$relig) <- c('protestant', 'total', 'cath', 'other')

## Long to wide

relig11_wide <- dcast(melt(relig11, id.vars = c("ags", "relig")), 
      ags ~ variable + relig, value.var = "value") %>%
  mutate_at(vars(c('tot_protestant', 'tot_cath', 'tot_other')),
            funs(. / tot_total)) %>%
  mutate(sum = tot_protestant + tot_cath + tot_other) %>%
  mutate(year = 2011) %>%
  dplyr::select(ags, tot_cath, tot_protestant, tot_other)

## Rename Columns

cnames <- c('ags', 'relig_cath_2011', 
            'relig_prot_2011', 'relig_other_2011')
colnames(relig11_wide) <- cnames

#### Income ####

inc <- read_xlsx('01_Raw_Data/Counties/gdp.xlsx', 
                 sheet = 'BIP',skip = 4)

## rename columns

cnames <- c('id', 'eucode', 'ags', 'land', 'n1', 'n2', 'n3', 'name',
            paste0('gdp_nominal_', c(1992, 1994:2016)))
colnames(inc) <- cnames

## Clean up 

inc <- inc %>%
  dplyr::select(one_of(c('ags', paste0('gdp_nominal_', c(1992, 1994:2016))))) %>%
  filter(!is.na(ags)) %>%
  filter(nchar(ags) == 5) %>%
  mutate_at(vars(paste0('gdp_nominal_', c(1992, 1994:1999))),
            as.numeric)

#### Wages ####

wage <- read_xlsx('01_Raw_Data/Counties/wages.xlsx', 
                 sheet = 'ANE insg.',skip = 4)

## rename columns

cnames <- c('id', 'eucode', 'ags', 'land', 'n1', 'n2', 'n3', 'name',
            paste0('wage_nominal', 2000:2016))
colnames(wage) <- cnames

## Clean up 

wage <- wage %>%
  dplyr::select(one_of(c('ags', paste0('wage_nominal', 2000:2016)))) %>%
  filter(!is.na(ags)) %>%
  filter(nchar(ags) == 5)

## clean up 

rm(relig11, unem_county)

#### Population #### 


