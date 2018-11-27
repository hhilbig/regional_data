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
         year = 2009) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem')))

#### 2010 ####

muni10 <- readxl::read_excel('01_Raw_Data/31122010_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni10) <- names_table

## Some cleanup

muni10 <- muni10 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart'))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2010) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem')))

#### 2011 ####

muni10 <- readxl::read_excel('01_Raw_Data/31122010_Auszug_GV.xls', 
                             sheet = 2, col_names = F, skip = 6)
colnames(muni10) <- names_table

## Some cleanup

muni10 <- muni10 %>% filter(!is.na(gem)) %>% 
  select(-one_of(c('txt', 'satzart'))) %>%
  mutate(ags = paste0(land, rb, kreis, gem),
         year = 2010) %>%
  select(-one_of(c('rb', 'kreis', 'vg', 'gem')))

#### 2012 ####

#### 2013 ####

#### 2014 ####

#### 2015 ####

#### 2016 ####

#### 2017 ####
