setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Library imports
library(openxlsx)
library(dplyr)
library(scales)
library(gridExtra)
library(survey)
library("gridExtra")
library(mice)
library(nnet)
library(VIM)
library(mice)
library(anesrake)
library(tidyverse)
library(tidyr)

##Select demographics and questions 
demographics <- c("gender","pmq","reg_status_aggregated","country_of_practice", "areas_practice","outside_uk_years",
  "medical_area_aggregated","inside_uk_years","inside_uk_years_aggregated",
  "patient_facing_role","age", "ai_type_use_qs", "dss_group", "dss_group","gen_group",
  "ai_use_frequency","used_gen_ai","used_ddss_ai", "used_ses_ai", "used_other_ai", "used_any_ai", 
  "interview_consent", "main_areas_of_work", "respondent_email", "frequency_single")
selected_demos <- c('reg_status_aggregated', 'gender', 'age', 'pmq')

## Read datasets
questions <- read.xlsx("../../Data/mappings.xlsx")
questions <- questions$Question
dataset <- read_csv("../../Data/raw_file.csv", show_col_types = FALSE)
dataset <- dataset %>% dplyr::select(all_of(c(demographics,questions)))

##clean datasets
dataset[dataset$pmq=='Outside the UK and European Economic Area','pmq'] <- 'IMG'
dataset[dataset$pmq=='European Economic Area (excluding the UK)','pmq'] <- 'EEA'
dataset[!is.na(dataset$gender) & dataset$gender=='Man','gender'] <- 'Male'
dataset[!is.na(dataset$gender) & dataset$gender=='Woman','gender'] <- 'Female'
dataset[dataset$age=='Prefer not to say', 'age'] <- NA_character_ 
dataset <- dataset %>% mutate(
  age_aggregated = case_when(
    age %in% c("Under 30","30-39") ~ "Under 40",
    age %in% c("40-49") ~"40-49",
    age %in% c("50-59", "60 years and over")~ "50+",
    TRUE~age))

#preliminary statistics before weighting and imputation
basic_statistics <- dataset %>% dplyr:: select(all_of(c(selected_demos,'medical_area_aggregated', 'used_any_ai')))%>%  
  reshape2::melt('used_any_ai') %>% group_by(variable,value) %>% summarise(n = n()) %>% 
  mutate(freq = round(n/sum(n)*100,2), sum_f = sum(freq),sum_n = sum(n)) 


### Impute missing variables 
dataset <- kNN(dataset, variable = c('reg_status_aggregated','gender','age') ,  k = 5)
dataset_stat_after_imputation <- dataset %>% dplyr:: select(all_of(c(selected_demos,'medical_area_aggregated', 'used_any_ai')))%>%  
  reshape2::melt('used_any_ai') %>% group_by(variable,value) %>% summarise(n = n()) %>% 
  mutate(freq = round(n/sum(n)*100,2), sum_f = sum(freq),sum_n = sum(n)) 

### Read GMC statistics 
gmc <- read.xlsx("../../Data/gmc_stats.xlsx")
gmc <- gmc %>% mutate(
  value = case_when(
    value %in% c('Under 25','25-29') ~ 'Under 30',
    value %in% c('30-34', '35-39') ~ '30-39',
    value %in% c('40-44', '45-49') ~ '40-49',
    value %in% c('50-54', '55-59') ~ '50-59',
    value %in% c('60-64', '65-69','70 and over') ~ '60 years and over', 
    TRUE~value))
gmc_stats <- gmc %>% group_by(variable, value) %>% summarise(n_gmc = sum(n_gmc)) %>%
  group_by(variable) %>% mutate(freq = n_gmc/sum(n_gmc)*100)


# change selected variables to factor variables in dataset
dataset$caseid <- 1:length(dataset$gender)
dataset[selected_demos] <- lapply(dataset[selected_demos], as.factor)

## Make the population of the target
reg_status_aggregated <- gmc_stats[gmc_stats$variable=='reg_status_aggregated',]$freq
names(reg_status_aggregated) <- levels(dataset$reg_status_aggregated)
gender <- gmc_stats[gmc_stats$variable=='gender',]$freq
names(gender) <- levels(dataset$gender)
age <- gmc_stats[gmc_stats$variable=='age',]$freq
names(age) <- levels(dataset$age)
pmq <- gmc_stats[gmc_stats$variable=='pmq',]$freq
names(pmq) <- levels(dataset$pmq)
targets <- list(reg_status_aggregated, gender, age, pmq)
names(targets) <- selected_demos

## Do the weighting and save to the file. 
anesrakefinder(targets, dataset, choosemethod = "total")
a <- dataset[c(selected_demos,'caseid')]
outsave <- anesrake(targets, a, caseid = dataset$caseid, verbose=TRUE)
summary(outsave)
dataset$weight <- unlist(outsave[1])
dataset_stat_after_weighting <- dataset %>% dplyr:: select(all_of(c(selected_demos,'weight'))) %>%  
  reshape2::melt('weight')%>% group_by(variable,value) %>% summarise(n_weighted = sum(weight), n = n()) %>% 
  mutate(freq = round(n/sum(n)*100,2), freq_weighted = round(n_weighted/sum(n_weighted)*100,2)) 

write_csv(dataset, "../../Data/clean_file_weighted.csv")
