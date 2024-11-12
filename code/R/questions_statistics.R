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


round_preserve_sum <- function(x, target_sum) {
  rounded <- floor(x)
  diff <- target_sum - sum(rounded)
  residuals <- x - rounded
  adjustment_indices <- order(residuals, decreasing = TRUE)[1:diff]
  rounded[adjustment_indices] <- rounded[adjustment_indices] + 1
  return(rounded)
}

## Read data and map questions to labels and clean data----
df <- read_csv("../../Data/clean_file_weighted.csv")
mappings <- read_xlsx("../../Data/mappings.xlsx")
questions <- mappings$Question
perception <- mappings[mappings$Label %in% c('perception', 'optimism'),]$Question
understanding <- mappings[mappings$Label %in% c('understanding'), ]$Question
responsibility <- mappings[mappings$Label %in% c('responsibility'), ]$Question 


## clean dataset
df <- df %>% mutate( age_aggregated = case_when(
  age %in% c("Under 30","30-39") ~ "Under 40",
  age %in% c("40-49") ~"40-49",
  age %in% c("50-59", "60 years and over")~ "50+",
  TRUE~age))

df[is.na(df)] <- "NA"
names(df)[names(df)=="medical_area_aggregated"] <- "Speciality"

df <- df %>%   mutate(net_optimsim = case_when(
                               aiuse_optimism %in% c("Very optimistic","Somewhat optimistic")  ~ "optimistic",
                               aiuse_optimism %in% c("Very pessimistic", "Somewhat pessimistic")  ~ "pessimistic",
                               TRUE~ aiuse_optimism
                             ), 
                      net_pmq = case_when(
                        pmq %in% c('EEA', 'IMG') ~ "non_UK",
                        TRUE ~ pmq
                      )
                      )
table(df$pmq)
df <- df %>%   mutate(across(all_of(questions), 
                             ~case_when(
                               .%in% c("Strongly agree", "Agree")   ~ "Agree",
                               .%in% c("Neither agree nor disagree", "Neutral")  ~ "Neutral",
                               .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                               .%in% c("Very optimistic","Somewhat optimistic")  ~ "Agree",
                               .%in% c("Very pessimistic", "Somewhat pessimistic")  ~ "Disagree",
                               TRUE~ "Neutral"
                             ))) 

#### produce results for all perception and responsibility questions #####
df_perception <- df  %>% dplyr::select(all_of(c(perception, responsibility,'weight'))) %>%  reshape2::melt('weight') %>% 
  group_by(variable,value) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
  mutate(freq = round(n/sum(n)*100,2), freq_weighted = round(n_weighted/sum(n_weighted)*100,2)) 

df_perception_ai <- df %>% filter(used_any_ai %in% "Yes")  %>% dplyr::select(all_of(c(perception, responsibility,'weight'))) %>%  reshape2::melt('weight') %>% 
  group_by(variable,value) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
  mutate(freq = round(n/sum(n)*100,2), freq_weighted_ai = round(n_weighted/sum(n_weighted)*100,2)) 

df_perception_not_ai <- df %>% filter(used_any_ai %in% "No")  %>% dplyr::select(all_of(c(perception, responsibility,'weight'))) %>%  reshape2::melt('weight') %>% 
  group_by(variable,value) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
  mutate(freq = round(n/sum(n)*100,2), freq_weighted_no_ai = round(n_weighted/sum(n_weighted)*100,2)) 

df_perception_gen_ai <- df %>% filter(used_any_ai %in% "Yes" & ai_type_use_qs %in% "Generative system" )  %>% dplyr::select(all_of(c(perception, responsibility,'weight'))) %>%  reshape2::melt('weight') %>% 
  group_by(variable,value) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
  mutate(freq = round(n/sum(n)*100,2), freq_weighted_no_ai = round(n_weighted/sum(n_weighted)*100,2))

df_perception_dss_ai <- df %>% filter(used_any_ai %in% "Yes" & ai_type_use_qs %in% "Diagnostic and decision support system" )  %>% dplyr::select(all_of(c(perception, responsibility,'weight'))) %>%  reshape2::melt('weight') %>% 
  group_by(variable,value) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
  mutate(freq = round(n/sum(n)*100,2), freq_weighted_no_ai = round(n_weighted/sum(n_weighted)*100,2)) 

df_perception <- merge(df_perception, df_perception_ai, by = c('variable','value'))
df_perception <- merge(df_perception, df_perception_not_ai, by = c('variable','value'))
colnames(df_perception)[1] <- 'Question' 
df_perception <- merge(df_perception, mappings)

## produce results for understanding questions divided by AI type users. 
## all AI users  
df_understanding <- df  %>% filter(used_any_ai %in% "Yes") %>% dplyr:: select(all_of(c(understanding,'weight'))) %>%  
  reshape2::melt('weight')%>% group_by(variable,value) %>% summarise(n_weighted = sum(weight), n = n()) %>% 
  mutate(freq_weighted = round(n_weighted/sum(n_weighted)*100,2), freq = round(n/sum(n)*100,2)) %>%
  select(!n_weighted)

## generative AI users  
df_gen <- df  %>% filter(ai_type_use_qs %in% "Generative system") %>% 
  dplyr::select(all_of(c(understanding,'weight'))) %>%  reshape2::melt('weight')%>% 
  group_by(variable,value) %>% summarise(n_weighted = sum(weight), n_gen = n()) %>% 
  mutate(freq_gen = round(n_gen/sum(n_gen)*100,2), freq_weighted_gen = round(n_weighted/sum(n_weighted)*100,2)) %>%
  select(!n_weighted)


## Decision support users 
df_dss <- df  %>% filter(ai_type_use_qs %in% "Diagnostic and decision support system") %>% 
  dplyr::select(all_of(c(understanding,'weight'))) %>%  reshape2::melt('weight')%>% 
  group_by(variable,value) %>% summarise(n_dss = n(), n_weighted = sum(weight)) %>% 
  mutate(freq_dss = round(n_dss/sum(n_dss)*100,2), freq_weighted_dss = round(n_weighted/sum(n_weighted)*100,2)) %>%
  select(!n_weighted)

### merge all data frames for understanding questions
df_understanding <- merge(df_understanding, df_gen, by = c('variable','value'))
df_understanding <- merge(df_understanding, df_dss, by = c('variable','value'))

#######generate the output excel sheet and save question results. 
OUT <- createWorkbook()
addWorksheet(OUT, "understanding")
addWorksheet(OUT, "perception")
writeData(OUT, sheet = "understanding", x = df_understanding)
writeData(OUT, sheet = "perception", x = df_perception)
saveWorkbook(OUT,  "../../Data/question_stat.xlsx")

#Scenario question statistics ----

#Num doctors responding

response_labels <- data.frame (response = c("Would document reasons/capture audit trial/rerun, check and review AI data, if possible", "Ignore/proceed with own clincial judgement",
                           "Follow AI", "Discuss with colleagues/senior consultant/MDT", 
                           "Discuss with patient/involve patient", 
                           "Follow up with AI developer, data team or other third party/flag alarm/raise concerns", 
                           "Double check my own reasoning/question myself/establish my reasons to disagree/cause concern", 
                           "Check internal or external guidelines/research/look for trial or assurance guidance/research literature"),
              labels = c("Check data and rerun system", 
                         "Proceed with my own clincial judgement", 
                         "Follow the AI judgment", 
                         "Discuss with colleagues", 
                         "Discuss with patient", 
                         "Follow up with AI developer and raise concerns", 
                         "Double check my own reasoning and question myself", 
                         "Check internal or external guidelines or research literature")
  )
df_scenario_all <- read_xlsx("../../Data/Scenario_question_analysis (version 1).xlsx") %>% 
  select(all_of(response_labels$response)) %>%  mutate_all(~ coalesce(., 0)) 


# Make column for those who mentioned ignoring without mentioning any other thing. 
cols_to_check <- setdiff(names(df_scenario_all), 'Ignore/proceed with own clincial judgement')
df_scenario_all$ignore <- ifelse(
  rowSums(df_scenario_all[, cols_to_check] == 0) == length(cols_to_check) & df_scenario_all$`Ignore/proceed with own clincial judgement` == 1, 
  1, 
  0
)

df_scenario <- 
  df_scenario_all %>%
  colSums(na.rm=T) %>%
  data.frame('response' = names(.), value=., row.names = NULL) %>%
  mutate(prop = value/nrow(df)) %>% #as prop of total people who gave an answer
  left_join(response_labels, by='response')




#### produce table for questions by demographics - Francis suggestion ######

### Understanding----
demographics <- c("reg_status_aggregated", "age_aggregated", "Speciality", "pmq", "gender", "used_gen_ai", "used_ddss_ai", "aiuse_optimism", "net_optimsim", "net_pmq")
ai_demo <- c("aiuse_optimism", "gender", "age_aggregated", "pmq", "reg_status_aggregated", "net_optimsim", "net_pmq")
OUT <- createWorkbook()
for (question in understanding){
  dd <- df %>% filter(used_any_ai %in% 'Yes')
  
  df_all <- dd %>% group_by_at(question) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    mutate(freq = round(n/sum(n)*100,2),freq_weighted = round(n_weighted/sum(n_weighted)*100,2), variable = "all",value = "all")
  
  df_question <- dd  %>% dplyr::select(all_of(c(demographics, question, 'weight'))) %>%  
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = demographics)%>% 
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2), 
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_all <- rbind(df_all, df_question)
  
  df_dds <- dd  %>% filter(used_ddss_ai %in% "Yes") %>% dplyr::select(all_of(c(ai_demo, question, 'weight'))) %>%
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = ai_demo)%>%
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>%
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2),
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_dds$variable <- paste0(df_dds$variable, "_dds")
  df_all <- rbind(df_all, df_dds)

  df_non_dds <- dd  %>% filter(used_ddss_ai %in% "No") %>% dplyr::select(all_of(c(ai_demo, question, 'weight'))) %>%
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = ai_demo)%>%
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>%
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2),
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_non_dds$variable <- paste0(df_non_dds$variable, "_non_dds")
  df_all <- rbind(df_all, df_non_dds)

  df_gen <- dd  %>% filter(used_gen_ai %in% "Yes") %>% dplyr::select(all_of(c(ai_demo, question, 'weight'))) %>%
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = ai_demo)%>%
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>%
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2),
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_gen$variable <- paste0(df_gen$variable, "_gen")
  df_all <- rbind(df_all, df_gen)

  df_non_gen <- dd  %>% filter(used_gen_ai %in% "No") %>% dplyr::select(all_of(c(ai_demo, question, 'weight'))) %>%
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = ai_demo)%>%
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>%
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2),
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_non_gen$variable <- paste0(df_non_gen$variable, "_non_gen")
  df_all <- rbind(df_all, df_non_gen)
  
  df_wide <- df_all
  names(df_wide)[names(df_wide)==question] <- "choice"
  df_wide$value <- paste0(df_wide$variable, "_", df_wide$value)
  df_1 <- pivot_wider(df_wide[,c('value', 'choice', 'freq_weighted')],
                      names_from = value,
                      values_from = c(freq_weighted),
                      values_fill = list(freq_weighted = 0))
  df_1$type <- "freq_weighted"
  df_2 <- pivot_wider(df_wide[,c('value', 'choice', 'freq')],
                      names_from = value,
                      values_from = freq,
                      values_fill = list(freq = 0))
  df_2$type <- "freq"
  df_3 <- pivot_wider(df_wide[,c('value', 'choice', 'n')],
                      names_from = value,
                      values_from = n,
                      values_fill = list(n = 0))
  df_3$type <- "n"
  
  df_4 <- pivot_wider(df_wide[,c('value', 'choice', 'n_weighted')],
                      names_from = value,
                      values_from = n_weighted,
                      values_fill = list(n_weighted = 0))
  df_4$type <- "n_weighted"
  
  df_wide <- rbind(df_1, rbind(rbind(df_2,df_3),df_4))
  addWorksheet(OUT, question)
  writeData(OUT, sheet = question, x = df_wide)
}
saveWorkbook(OUT,  "../../Data/Francis_stats_understanding.xlsx")


### Perceptions----
original_demographics <- c("reg_status_aggregated","country_of_practice", "age", "age_aggregated", "Speciality", "pmq", "gender", 
                           "inside_uk_years","inside_uk_years_aggregated","patient_facing_role", "used_any_ai", "used_gen_ai", "used_ddss_ai", "net_optimsim", "net_pmq")
ai_demo <- c("aiuse_optimism", "gender", "age_aggregated", "pmq", "reg_status_aggregated", "net_optimsim", "net_pmq")
OUT <- createWorkbook()
for (question in c(perception,responsibility)){
  dd <- df
  if (question!="aiuse_optimism")
    demographics <- c(original_demographics, "aiuse_optimism")
  else
    demographics <- original_demographics
  
  df_all <- dd %>% group_by_at(question) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    mutate(freq = round(n/sum(n)*100,2), freq_weighted = round(n_weighted/sum(n_weighted)*100,2), variable = "all",value = "all")
  
  df_question <- dd  %>% dplyr::select(all_of(c(demographics, question, 'weight'))) %>%  
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = demographics)%>% 
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2), 
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_all <- rbind(df_all, df_question)
  
  df_ai_users <- dd  %>% filter(used_any_ai %in% "Yes") %>% dplyr::select(all_of(c(ai_demo, question, 'weight'))) %>%  
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = ai_demo)%>% 
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2), 
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_ai_users$variable <- paste0(df_ai_users$variable, "_ai_users")
  df_all <- rbind(df_all, df_ai_users)
  
  df_non_ai_users <- dd  %>% filter(used_any_ai %in% "No") %>% dplyr::select(all_of(c(ai_demo, question, 'weight'))) %>%  
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = ai_demo)%>% 
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2), 
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_non_ai_users$variable <- paste0(df_non_ai_users$variable, "_non_ai_users")
  df_all <- rbind(df_all, df_non_ai_users)
  
  df_wide <- df_all
  names(df_wide)[names(df_wide)==question] <- "choice"
  df_wide$value <- paste0(df_wide$variable, "_", df_wide$value)
  #df_wide <- subset(df_wide, select = c(value, choice, freq_weighted, freq, n))
  df_1 <- pivot_wider(df_wide[,c('value', 'choice', 'freq_weighted')], 
                         names_from = value,
                         values_from = c(freq_weighted), 
                         values_fill = list(freq_weighted = 0))
  df_1$type <- "freq_weighted"
  df_2 <- pivot_wider(df_wide[,c('value', 'choice', 'freq')], 
                         names_from = value,
                         values_from = freq, 
                         values_fill = list(freq = 0))
  df_2$type <- "freq"
  df_3 <- pivot_wider(df_wide[,c('value', 'choice', 'n')], 
                      names_from = value,
                      values_from = n, 
                      values_fill = list(n = 0))
  df_3$type <- "n"
  
  df_4 <- pivot_wider(df_wide[,c('value', 'choice', 'n_weighted')], 
                      names_from = value,
                      values_from = n_weighted, 
                      values_fill = list(n_weighted = 0))
  df_4$type <- "n_weighted"
  
  df_wide <- rbind(rbind(df_1, rbind(df_2,df_3)), df_4)
  addWorksheet(OUT, question)
  writeData(OUT, sheet = question, x = df_wide)
}
write_xlsx(df_wide, "../../Data/ai_use_optimim.xlsx")

saveWorkbook(OUT,  "../../Data/Francis_stats_perceptions.xlsx")
write_xlsx(df_question_by_demo, "../../Data/question_demographics_stat.xlsx")

#### AI use questions ----
demographics <- c("reg_status_aggregated","country_of_practice", "age", "age_aggregated", "Speciality", "pmq", "gender", 
                           "inside_uk_years","inside_uk_years_aggregated","patient_facing_role", "aiuse_optimism", "net_optimsim", "net_pmq")
AI_use = c("used_any_ai", "used_ddss_ai", "used_gen_ai")
OUT <- createWorkbook()
for (question in AI_use){
  
  dd <- df
  df_all <- dd %>% group_by_at(question) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    mutate(freq = round(n/sum(n)*100,2),freq_weighted = round(n_weighted/sum(n_weighted)*100,2), variable = "all",value = "all")
  
  df_question <- dd  %>% dplyr::select(all_of(c(demographics, question, 'weight'))) %>%  
    reshape2::melt(id.vars = c(question, 'weight'), measure.vars = demographics)%>% 
    group_by_at(c('variable','value',question)) %>% summarise(n = n(), n_weighted = sum(weight)) %>% 
    group_by_at(c('value', 'variable')) %>%
    mutate(freq = round(n/sum(n)*100,2), 
           freq_weighted = round(n_weighted/sum(n_weighted)*100,2))
  df_all <- rbind(df_all, df_question)
  
  df_wide <- df_all
  names(df_wide)[names(df_wide)==question] <- "choice"
  df_wide$value <- paste0(df_wide$variable, "_", df_wide$value)
  df_1 <- pivot_wider(df_wide[,c('value', 'choice', 'freq_weighted')],
                      names_from = value,
                      values_from = c(freq_weighted),
                      values_fill = list(freq_weighted = 0))
  df_1$type <- "freq_weighted"
  df_2 <- pivot_wider(df_wide[,c('value', 'choice', 'freq')],
                      names_from = value,
                      values_from = freq,
                      values_fill = list(freq = 0))
  df_2$type <- "freq"
  df_3 <- pivot_wider(df_wide[,c('value', 'choice', 'n')],
                      names_from = value,
                      values_from = n,
                      values_fill = list(n = 0))
  df_3$type <- "n"
  df_4 <- pivot_wider(df_wide[,c('value', 'choice', 'n_weighted')],
                      names_from = value,
                      values_from = n_weighted,
                      values_fill = list(n_weighted = 0))
  df_4$type <- "n_weighted"
  
  df_wide <- rbind(df_1, rbind(rbind(df_2,df_3),df_4))
  addWorksheet(OUT, question)
  writeData(OUT, sheet = question, x = df_wide)

}
saveWorkbook(OUT,  "../../Data/Francis_stats_AI_use.xlsx")






