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

## ## ## ##

#### 2009 ####

muni09 <- readxl::read_excel('01_Raw_Data/31122009_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)

## Rename

names_table <- c('satzart', 'txt', 'land', 'rb', 'kreis', 
                 'vg', 'gem', 'name', 'area_km2', 'pop_tot',
                 'pop_male', 'pop_female', 'pop_density_km2',
                 'zip', 'lon', 'lat')
colnames(muni09) <- names_table

## Some cleanup

muni09 <- muni09 %>% filter(!is.na(gem)) %>%              # Only gemeinden
  dplyr::select(-one_of(c('txt', 'satzart'))) %>%                # Drop some vars
  mutate(ags = paste0(land, rb, kreis, gem),              # Gen AGS
         year = 2009,                                     # Add Year
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%  # Add east dummy 
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%      # Drop more vars
  filter(!pop_tot == 0)                                   # Drop 'fake' Gemeinden

#### 2010 ####

muni10 <- readxl::read_excel('01_Raw_Data/31122010_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni10) <- names_table

## Some cleanup

muni10 <- muni10 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart'))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2010,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%      # Drop more vars
  filter(!pop_tot == 0)                            

#### 2011 ####

muni11 <- readxl::read_excel('01_Raw_Data/31122011_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni11) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni11 <- muni11 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2011,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%      # Drop more vars
  filter(!pop_tot == 0)                            

#### 2012 ####

muni12 <- readxl::read_excel('01_Raw_Data/31122012_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni12) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni12 <- muni12 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2012,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))  %>%      # Drop more vars
  filter(!pop_tot == 0)                            

#### 2013 ####

muni13 <- readxl::read_excel('01_Raw_Data/31122013_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni13) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni13 <- muni13 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2013,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))  %>%      # Drop more vars
  filter(!pop_tot == 0)                            

#### 2014 ####

muni14 <- readxl::read_excel('01_Raw_Data/31122014_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni14) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni14 <- muni14 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2014,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))  %>%      # Drop more vars
  filter(!pop_tot == 0)                            

#### 2015 ####

muni15 <- readxl::read_excel('01_Raw_Data/31122015_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni15) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni15 <- muni15 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2015,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))  %>%      # Drop more vars
  filter(!pop_tot == 0)                            

#### 2016 ####

muni16 <- readxl::read_excel('01_Raw_Data/31122016_Auszug_GV.xlsx', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni16) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni16 <- muni16 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2016,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))  %>%      # Drop more vars
  filter(!pop_tot == 0)                            

#### 2017 ####

muni17 <- readxl::read_excel('01_Raw_Data/31122017_Auszug_GV.xlsx', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni17) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni17 <- muni17 %>% filter(!is.na(gem)) %>% 
  dplyr::select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2017,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  dplyr::select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), 
         lat = as.numeric(lat)) %>%      # Drop more vars
  filter(!pop_tot == 0)                            

## Make a list of the municipalities ## 

list_muni <- list(muni09, muni10, muni11, muni12, muni13,
                  muni14, muni15, muni16, muni17)



#### Add Unemployment data #### 

dir_contents <- dir('01_Raw_Data/Unemployment/')

files_unem_csv <- dir_contents[str_detect(string = dir_contents,
                                          pattern = '.csv')]

## Load 2017

unem17 <- readxl::read_xlsx(path = '01_Raw_Data/Unemployment/Jahreszahlen 2017-Gemeinde.xlsx',
                            sheet = 6, 
                            skip = 9, 
                            col_names = F)

## Set colnames

cnames <- c('ags', 'name', 'unem_tot', 
            paste0('other', 1:4), 'unem_foreigner',
            paste0('other', 5:28))
colnames(unem17) <- cnames

## Clean up

unem17 <- unem17 %>% filter(nchar(ags) == 8) %>%
  dplyr::select(-one_of(paste0('other', 1:28))) %>% 
  dplyr::select(ags, unem_tot, unem_foreigner)

## Load 2016

unem16 <- readxl::read_xlsx(path = '01_Raw_Data/Unemployment/2016JZ-gem21_d_0.xlsx',
                            sheet = 6, 
                            skip = 9, 
                            col_names = F)

## Set colnames

cnames <- c('ags', 'name', 'unem_tot', 
            paste0('other', 1:4), 'unem_foreigner',
            paste0('other', 5:28))
colnames(unem16) <- cnames

## Clean up

unem16 <- unem16 %>% filter(nchar(ags) == 8) %>%
  dplyr::select(-one_of(paste0('other', 1:28))) %>% 
  dplyr::select(ags, unem_tot, unem_foreigner)

## Make this a function

cleanup_unem <- function(filepath) { 
  df <- read.csv(file = filepath, 
                 header = F, 
                 stringsAsFactors = F) %>% 
    slice(10:n())
  
  ## Replace all commas with full stops
  
  df[, 3:32] <- apply(df[, 3:32], 2, function(x) {
    as.numeric(str_replace_all(x, ',', ''))
  })
  
  ## Set colnames
  
  cnames <- c('ags', 'name', 'unem_tot', 
              paste0('other', 1:4), 'unem_foreigner',
              paste0('other', 5:28))
  colnames(df) <- cnames
  
  ## Clean up
  
  df <- df %>% filter(nchar(ags) == 8) %>%
    dplyr::select(-one_of(paste0('other', 1:28))) %>% 
    dplyr::select(ags, unem_tot, unem_foreigner)
  
  ## Return DF
  
  df
}

## Run the function on all filepaths

fpaths <- paste0("01_Raw_Data/Unemployment/", files_unem_csv)

## Function:

list_dfs <- pblapply(fpaths, cleanup_unem)
names(list_dfs) <- 2009:2015

## Add the last two years to the list

list_dfs[['2016']] <- unem16
list_dfs[['2017']] <- unem17

## Quick check: Are AGS unique?

cat('This has to be TRUE for all years')
sapply(list_dfs, function(x) sum(duplicated(x$ags)) == 0)

## Looks good, ready for merging 

out_final_list <- pblapply(1:9, function(i) {
  df <- left_join(list_muni[[i]], list_dfs[[i]])
  
  ## Get unem / capita
  
  df <- df %>% mutate(unem_capita = unem_tot / pop_tot)
  
  ## Return this
  
  df
}) 

## Check max of unem / capita per year (sanity check)

cat('Max unemployment per capita (THESE SHOULD BE < 1): ',
    round(sapply(out_final_list, function(x) max(x$unem_capita, na.rm = T)), 3))

## Check the means 

cat('Mean unemployment per capita (THESE SHOULD BE similar): ',
    round(sapply(out_final_list, function(x) mean(x$unem_capita, na.rm = T)), 3))

## Check the SDs 

cat('SD of  unemployment per capita (THESE SHOULD BE similar): ',
    round(sapply(out_final_list, function(x) sd(x$unem_capita, na.rm = T)), 3))

## Looks ok I guess

#### Save the data #### 

## List of years

years <- 2009:2017

## ## ## Save this

for (j in 1:length(years)) {
  
  ## Write
  
  write.csv(out_final_list[[j]], 
            file = paste0('02_Clean_Data/muni_', years[j], '.csv'),
            row.names = F, 
            fileEncoding = 'UTF-8')
  
}



