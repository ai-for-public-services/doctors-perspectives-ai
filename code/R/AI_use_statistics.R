#Library imports
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tidyr)
library(openxlsx)
library(scales)
library(gridExtra)
library(survey)
library("gridExtra")
library(writexl)
library(dplyr)
library(readxl)
library(weights)
library(survey)

## Read data and map questions to labels and clean data
df <- read_csv("../../Data/clean_file_weighted.csv")
mappings <- read_xlsx("../../Data/mappings.xlsx")
questions <- mappings$Question
perception <- mappings[mappings$Label %in% c('perception', 'optimism'),]$Question
understanding <- mappings[mappings$Label %in% c('understanding'), ]$Question
responsibility <- mappings[mappings$Label %in% c('responsibility'), ]$Question 

#clean dataset
df <- df %>% mutate( age_aggregated = case_when(
  age %in% c("Under 30","30-39") ~ "Under 40",
  age %in% c("40-49") ~"40-49",
  age %in% c("50-59", "60 years and over")~ "50+",
  TRUE~age))
df <- df %>%   mutate(across(all_of(questions), 
                             ~case_when(
                               .%in% c("Strongly agree", "Agree")   ~ "Agree",
                               .%in% c("Neither agree nor disagree", "Neutral")  ~ "Neutral",
                               .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                               .%in% c("Very pessimistic", "Somewhat pessimistic")  ~ "Pessimistic",
                               .%in% c("Very optimistic","Somewhat optimistic")  ~ "Optimistic",
                               TRUE~ "Neutral"
                             ))) 
df$used_ses_ai <- ifelse(df$used_ses_ai=='Yes' | df$used_other_ai=='Yes','Yes','No')
df$ai_use_frequency <- ifelse(is.na(df$ai_use_frequency) & df$used_any_ai=='Yes', 'Other', df$ai_use_frequency)


#select demographics. 
selected_demos <- c('reg_status_aggregated', 'gender', 'age', 'pmq', 'medical_area_aggregated')
demographics <- c("reg_status_aggregated", "age", "medical_area_aggregated", 
                  "inside_uk_years_aggregated",  "pmq", "gender", "used_any_ai", 
                  "used_gen_ai", "used_ddss_ai", "used_ses_ai", "ai_type_use_qs")

#### produce table for demographics ######
## Table 1 in text and appendix
df_demographics <- df %>% dplyr:: select(all_of(c(demographics,'weight'))) %>% 
  reshape2::melt('weight')%>% group_by(variable,value) %>% summarise(n_weighted = sum(weight), n = n()) %>% 
  mutate(freq = round(n/sum(n)*100,2), freq_weighted = round(n_weighted/sum(n_weighted)*100,2)) 
write_xlsx(df_demographics, "../../Data/demographics.xlsx")

## Table 2 - AI details
dss_details <- df %>% filter(ai_type_use_qs %in% 'Diagnostic and decision support system') %>% 
 group_by(dss_group) %>% summarise(n = sum(weight)) %>% mutate(freq_dss = round(n/sum(n)*100,2))

gen_details <- df %>% filter(ai_type_use_qs %in% 'Generative system') %>% 
  group_by(gen_group) %>% summarise(n = sum(weight)) %>% mutate(freq_gen = round(n/sum(n)*100,2))

ai_details <- full_join(dss_details, gen_details)
write_xlsx(ai_details, "../../Data/AI_types.xlsx")

## Table 3, AI use frequency
gen_freq <- df %>% filter(ai_type_use_qs %in% 'Generative system') %>% group_by(ai_use_frequency) %>%
  summarise(n_w = sum(weight), n = n()) %>% mutate(freq_gen_weighted = round(n_w/sum(n_w)*100,2), freq_gen = n/sum(n)*100) %>%
  select(ai_use_frequency,freq_gen_weighted,freq_gen)

dss_freq <- df %>% filter(ai_type_use_qs %in% 'Diagnostic and decision support system') %>% group_by(ai_use_frequency) %>%
  summarise(n_w = sum(weight), n= n()) %>% mutate(freq_dss_weighted = round(n_w/sum(n_w)*100,2), freq_dss = n/sum(n)*100) %>%
  select(ai_use_frequency,freq_dss_weighted, freq_dss)

ai_freq <- df %>% filter(used_any_ai %in% 'Yes') %>% group_by(ai_use_frequency) %>%
  summarise(n_w = sum(weight), n = n()) %>% mutate(freq_ai_weighted = round(n_w/sum(n_w)*100,2),freq_ai = n/sum(n)*100) %>%
  select(ai_use_frequency,freq_ai_weighted, freq_ai)

ai_freq <- merge(ai_freq, gen_freq)
ai_freq <- merge(ai_freq, dss_freq)
write_xlsx(ai_freq, "../../Data/frequency.xlsx")

### AI use broken by demographics: Any AI, gen AI, DDSS AI ######
## Table 4
demographic = c("age_aggregated", "medical_area_aggregated", "reg_status_aggregated", "gender", "pmq")
ai_demo = data_frame()
for (j in 1:5)
{
  temp <- df %>% group_by_at(c('used_any_ai',demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
    group_by_at(demographic[j]) %>% mutate(freq_ai = round(n/sum(n)*100,2), freq_weighted_ai = round(n_w/sum(n_w)*100,2)) %>% filter(used_any_ai =='Yes') %>%
    mutate(variable = demographic[j])
  colnames(temp)[colnames(temp) == demographic[j]] <- "value"
  ai_demo <- rbind(ai_demo,temp) 
}
ai_demo <- ai_demo %>% select(variable,value,freq_ai, freq_weighted_ai)
gen_ai_demo = data_frame()
for (j in 1:5)
{
  temp <- df %>% group_by_at(c('used_gen_ai',demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
    group_by_at(demographic[j]) %>% mutate(freq_gen= round(n/sum(n)*100,2), freq_weighted_gen = round(n_w/sum(n_w)*100,2)) %>% filter(used_gen_ai =='Yes') %>%
    mutate(variable = demographic[j])
  colnames(temp)[colnames(temp) == demographic[j]] <- "value"
  gen_ai_demo <- rbind(gen_ai_demo,temp) 
}
gen_ai_demo <- gen_ai_demo %>% select(variable,value,freq_gen, freq_weighted_gen)
dds_ai_demo = data_frame()
for (j in 1:5)
{
  temp <- df %>% group_by_at(c('used_ddss_ai',demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
    group_by_at(demographic[j]) %>% mutate(freq_dds= round(n/sum(n)*100,2), freq_weighted_dds = round(n_w/sum(n_w)*100,2)) %>% filter(used_ddss_ai =='Yes') %>%
    mutate(variable = demographic[j]) 
  colnames(temp)[colnames(temp) == demographic[j]] <- "value"
  dds_ai_demo <- rbind(dds_ai_demo,temp) 
}
dds_ai_demo <- dds_ai_demo %>% select(variable,value,freq_dds, freq_weighted_dds)
dds_ai_demo <- rbind(dds_ai_demo, c(variable = "Psychiatry", value = "medical_area_aggregated", freq_dds = double(0), freq_weighted_dds = double(0)))

ai_demo <- merge(ai_demo,gen_ai_demo , all = TRUE)
ai_demo <- merge(ai_demo, dds_ai_demo, all = TRUE)
write_xlsx(ai_demo, "../../Data/ai_use_demographics.xlsx")

