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

#### Unemployment ####

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
  filter(nchar(ags) == 5) %>%
  dplyr::select(-name)

## Split this into yearly data frames

list_df_unem <- lapply(unique(unem_county$year), function(y) {
  temp <- unem_county %>% filter(year == y) 
  
  ## Rename columns
  
  colnames(temp)[3:15] <- paste0(colnames(temp)[3:15], '_', unique(temp$year))
  
  ## return this (w/o year)
  
  temp %>% dplyr::select(-year)
}) 

names(list_df_unem) <- paste0('year_', unique(unem_county$year))

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

## Drop NA

relig11_wide$na_all <- relig11_wide %>% select(contains('relig')) %>%
  apply(., 1, function(x) sum(is.na(x)) == 3)

## Remove all na cases

relig11_wide <- relig11_wide %>%
  filter(na_all == F) %>%
  dplyr::select(-contains('na_'))

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

pop <- read_delim('01_Raw_Data/Counties/pop_dec_2011.csv',
                  delim = ';', skip = 6) %>%
  filter(X3 == 'Insgesamt') %>%
  filter(nchar(X1) == 5) %>%
  dplyr::select(-one_of('X3'))

## Rename 

colnames(pop) <- varlist <- c('ags', 'name', 
                   'pop_total',
                   'pop_total_male', 
                   'pop_total_female',
                   'pop_native',
                   'pop_native_male',
                   'pop_native_female',
                   'pop_foreign',
                   'pop_foreign_male',
                   'pop_foreign_female')

## Clean up 

pop <- pop %>%
  mutate_at(vars(varlist[-1:-2]),
            as.numeric) %>%
  dplyr::select(-one_of('name')) %>%
  mutate(pop_foreign_share = pop)

#### Population v2 ####

pop2 <- read_delim('01_Raw_Data/Counties/pop_kreise_from_2009.csv', skip = 5,
                 delim = ';')

colnames(pop2) <- c('ags', 'name', paste0('pop_', 2009:2016))

## Clean up

pop2 <- pop2 %>% dplyr::select(-one_of('name')) %>%
  mutate_at(vars(paste0('pop_', 2009:2016)), 
            as.numeric)

pop2$na_all <- pop2 %>% select(contains('pop')) %>%
  apply(., 1, function(x) sum(is.na(x)) == 8)

## Remove all na cases

pop2 <- pop2 %>%
  filter(na_all == F) %>%
  dplyr::select(-contains('na_'))

#### Combining the complete data ####

## use the income / gdp files as the base files

sum(wage$ags %in% inc$ags) / nrow(inc)
sum(pop2$ags %in% inc$ags) / nrow(pop2)
sum(relig11_wide$ags %in% inc$ags) / nrow(relig11_wide)

## Get the ZHB data

regional_final <- inc %>%
  left_join(., wage) %>%
  left_join(., pop2) %>%
  left_join(., relig11_wide) %>%
  left_join(., list_df_unem[['year_2011']])

## Save for now

write_csv(regional_final, path = '02_Clean_Data/county.csv')

## This is how we can speed up the left_join for a list of data frames

library(tidyverse)
bla <- reduce(list_df_unem, full_join, by = 'ags')
  


 


