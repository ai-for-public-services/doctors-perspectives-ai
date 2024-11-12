## Library imports
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(openxlsx)
library(readxl)
library(dplyr)
library("gridExtra")
library(survey)
library(splitstackshape)

## Read data and clean it. 
df <- read_csv("../../Data/clean_file_April.csv")
questions <- read_xlsx("../../Data/mappings.xlsx")$Question

##clean the data frame 
df[df["age"]=="Under 30", "age"] <- "30 or under"
df[df["pmq"]=="European Economic Area (excluding the UK)", "pmq"] <- "EEA"
df[df["pmq"]=="Outside the UK and European Economic Area", "pmq"] <- "IMG"


# Statistics for questions and division by demographics fro interview_ai, ai, interview and all
#############################################################################
dems <- c("gender", "pmq", "reg_status_aggregated", "age_aggregated", "medical_area_aggregated","id")

dd <- df %>%   mutate(across(all_of(questions), 
                             ~case_when(
                               .%in% c("Strongly agree", "Agree")   ~ "Agree",
                               .%in% c("Neither agree nor disagree", "Neutral")  ~ "Neutral",
                               .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                               .%in% c("Very pessimistic", "Somewhat pessimistic")  ~ "Pessimistic",
                               .%in% c("Very optimistic","Somewhat optimistic")  ~ "Optimistic",
                               TRUE~NA_character_
                             )))


### demographics for AI users consented to interview. 
df1 <- dd  %>% dplyr:: filter(interview_consent %in% "Yes" & used_any_ai %in% "Yes") %>%   dplyr:: select(all_of(c(questions, dems))) %>%  dplyr::mutate(number= n()) %>% reshape2::melt('number')
df5 <- df1 %>% group_by(variable,value)  %>% summarise(n_interview_ai =n()) %>% mutate(freq_interview_ai = n_interview_ai/sum(n_interview_ai)*100) 
write_csv(df5, "../../Data/opinion.csv")


#demographics for all
df1 <- df  %>% dplyr:: select(all_of(c(questions, dems))) %>% dplyr::mutate(number= n()) %>% reshape2::melt('number')
df2 <- df1 %>% group_by(variable,value) %>% summarise(n_all=n()) %>% mutate(freq_all= n_all/sum(n_all)*100) 
write_csv(df2, "../../Data/opinion.csv")

#demographics for those consented to interview
df1 <- df  %>% dplyr:: select(all_of(c(selections, dems))) %>% dplyr:: filter(interview_consent %in% "Yes") %>% dplyr::mutate(number= n()) %>% reshape2::melt('number')
df3 <- df1 %>% group_by(variable,value) %>% summarise(n_interview=n()) %>% mutate(freq_interview = n_interview/sum(n_interview)*100) 


#demographics for AI users
df1 <- df  %>% dplyr:: select(all_of(c(selections, dems))) %>%  dplyr:: filter(used_any_ai %in% "Yes") %>% dplyr::mutate(number= n()) %>% reshape2::melt('number')
df4 <- df1 %>% group_by(variable,value) %>% summarise(n_ai=n()) %>% mutate(freq_ai = n_ai/sum(n_ai)*100) 


##merge and write all. 
all_df <- merge(df2, df3, by = c("variable","value"), all = TRUE)
all_df <- merge(all_df, df4, by = c("variable","value"), all = TRUE)
all_df <- merge(all_df, df5, by = c("variable","value"), all = TRUE)
write_csv(all_df, "../../Data/demographics.csv")

##########################Sampling########################################
df <- read_csv("../../Data/cleaned_analytic_file_for_viz_120324.csv", show_col_types = FALSE)
dems <- c("gender", "pmq", "reg_status_aggregated", "age_aggregated", "medical_area_aggregated","respondent_email", "view")
selection <- c("aiuse_perception_3",	"aiuse_impact_1",	"aiuse_impact_2",	
               "aiuse_impact_3",	"ai_use_training")

### Get view points..........................
temp <- df %>%   mutate(across(all_of(selection),
                              ~case_when(
                                .%in% c("Strongly agree", "Very optimistic")   ~ 2,
                                .%in% c("Agree", "Somewhat optimistic")  ~ 1,
                                .%in% c("Neutral", "Neither agree nor disagree")  ~ 0,
                                .%in% c("Disagree", "Somewhat pessimistic")  ~ -1,
                                .%in% c("Strongly disagree", "Very pessimistic")  ~ -2,
                                TRUE ~ 0 
                              ))) 
score <- rowSums(temp[, selection])
df <- df %>% mutate(
  score = score, 
  view = case_when(
    score > 0 ~ "positive",
    score < 0 ~ "negative",
    score ==0 ~ "Neutral"
  )
)

dd <- df  %>% dplyr:: filter(interview_consent %in% "Yes" & used_any_ai %in% "Yes") %>% 
  dplyr:: select(all_of(c(dems, selection))) 

## select charactristics to balance out in the sample
selected_columns <- c('gender', 'medical_area_aggregated')
## Remove NA for special columns and generate the stratum column. 
interview_data <- dd[complete.cases(dd[, c(selected_columns, 'medical_area_aggregated')]), ]
interview_data$combined_stratum <- do.call(paste, c(interview_data[selected_columns], sep = "_"))

## determine the strata sizes - number of samples we want to choose from each stratum
strata_sizes <- c("Man_Anaesthetics and Intensive Care Medicine" = 	2, 
"Man_Emergency Medicine" = 	2,
"Man_General Practice" = 2,
"Man_Medicine" = 	2,
"Man_Paediatrics" = 	1,
"Man_Psychiatry"=	1,
"Man_Radiology"	=1,
"Man_Surgery" = 	2,
"Woman_Anaesthetics and Intensive Care Medicine" = 	1,
"Woman_Emergency Medicine" =	1,
"Woman_General Practice" = 	2,
"Woman_Medicine" = 	3,
"Woman_Paediatrics" =	1,
"Woman_Psychiatry" = 	1,
"Woman_Radiology" =	1,
"Woman_Surgery" = 	3)
sampled_data <- stratified(interview_data, group = "combined_stratum", size = strata_sizes)

## get statistics from the sample and the original data
stat_dems <- c('gender','pmq','medical_area_aggregated','age_aggregated','view', 'reg_status_aggregated')
## get stat from the dataset for selected features
dataset_stat <- interview_data  %>% dplyr::select(all_of(stat_dems)) %>% dplyr::mutate(number= n()) %>% 
  reshape2::melt('number') %>% group_by(variable,value)  %>% summarise(n_interview_ai =n())
## get stat from the chosen sample for selected features
sample_stat <- sampled_data  %>% dplyr::select(all_of(c(stat_dems))) %>% dplyr::mutate(number= n()) %>% reshape2::melt('number') %>% 
  group_by(variable,value)  %>% summarise(n =n())

## combine the sample with statistics and write them to an excel file. 
all_df <- rbind(sampled_data, sample_stat, fill = TRUE)
all_df <- rbind(all_df, dataset_stat, fill = TRUE)
write_csv(all_df, "../../Data/sample.csv")

#####take the sample generated and chose 10 from that..... 
initial_sample <- read_csv("../../Data/sample.csv")[1:26,c(dems, selection)]
email_sample <- stratified(initial_sample, group = c("gender","age_aggregated"), size = 2)
write_csv(all_df, "../../Data/email_sample.csv")

#email_sample <- initial_sample[sample(nrow(initial_sample), 10), ]


