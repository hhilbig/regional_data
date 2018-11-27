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

muni09 <- muni09 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart'))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2009,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem')))

#### 2010 ####

muni10 <- readxl::read_excel('01_Raw_Data/31122010_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni10) <- names_table

## Some cleanup

muni10 <- muni10 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart'))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2010,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem')))

#### 2011 ####

muni11 <- readxl::read_excel('01_Raw_Data/31122011_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni11) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni11 <- muni11 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2011,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem')))

#### 2012 ####

muni12 <- readxl::read_excel('01_Raw_Data/31122012_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni12) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni12 <- muni12 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2012,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) 

#### 2013 ####

muni13 <- readxl::read_excel('01_Raw_Data/31122013_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni13) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni13 <- muni13 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2013,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) 

#### 2014 ####

muni14 <- readxl::read_excel('01_Raw_Data/31122014_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni14) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni14 <- muni14 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2014,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) 

#### 2015 ####

muni15 <- readxl::read_excel('01_Raw_Data/31122015_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni15) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni15 <- muni15 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2015,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) 

#### 2016 ####

muni16 <- readxl::read_excel('01_Raw_Data/31122016_Auszug_GV.xlsx', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni16) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni16 <- muni16 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
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
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) 

#### 2017 ####

muni17 <- readxl::read_excel('01_Raw_Data/31122017_Auszug_GV.xlsx', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni17) <- c(names_table, paste0('other', 1:4))

## Some cleanup

muni17 <- muni17 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart', paste0('other', 1:4)))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2017,
         east = ifelse(as.numeric(land > 10), 1, 0)) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem'))) %>%
  mutate(lon = str_replace_all(string = lon, 
                               pattern = ',', 
                               replacement = '\\.'),
         lat = str_replace_all(string = lat, 
                               pattern = ',', 
                               replacement = '\\.')) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))
