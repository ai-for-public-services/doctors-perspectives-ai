#Library imports ###########
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(mlogit)
library(tidyverse)
library(tidyr)
library(openxlsx)
library(dplyr)
library(scales)
library(gridExtra)
library(MASS)
library(survey)
library(nnet)
library("gridExtra")
library(aod)
library(car)
library(gam)
library(ResourceSelection)
library(stargazer)
library(pROC)
library(margins)
library(brant)
library(VGAM)
library(ordinal)
#library(ANOVA)

run_ordered_logit <- function(dd, formula, model_type = 'polr', question) {
  polr_model <- polr(formula, data = dd, weights = dd$weight, Hess = TRUE)
  clm_model <- clm(formula, data = dd, weights = dd$weight)
  print(question)
  brant_model <- brant(polr_model)
  brant_model <- cbind(rownames(brant_model), brant_model)
  
  if (question %in% c('ai_use_concerns','ai_use_explain'))
    vglm_model <- vglm(formula, data = dd, weights = dd$weight,
                       family = cumulative(parallel = FALSE ~ 1  + medical_area_aggregatedRadiology, reverse = TRUE))
  else if (question %in% understanding[1:6])
    vglm_model <- vglm(formula, data = dd, weights = dd$weight, family = cumulative(parallel = FALSE ~ 1 + pmq, reverse = TRUE))
  
  else if (question %in% c('aiuse_perception_1'))
    vglm_model <- vglm(formula, data = dd, weights = dd$weight,
                       family = cumulative(parallel = FALSE ~ 1  + used_any_ai + medical_area_aggregated, reverse = TRUE))
  else if (question %in% c('aiuse_perception_6'))
    vglm_model <- vglm(formula, data = dd, weights = dd$weight,
                       family = cumulative(parallel = FALSE ~ 1  + medical_area_aggregated + LED_SAS + gender, reverse = TRUE))
  else if (question %in% c('aiuse_perception_2', 'aiuse_perception_3'))
    vglm_model <- vglm(formula, data = dd, weights = dd$weight,
                       family = cumulative(parallel = FALSE ~ 1 + age + medical_area_aggregated, reverse = TRUE))
  else if (question %in% c('aiuse_perception_4', 'aiuse_perception_5'))
    vglm_model <- vglm(formula, data = dd, weights = dd$weight,
                       family = cumulative(parallel = FALSE ~ 1 + gender + LED_SAS, reverse = TRUE))
  else
    vglm_model <- vglm(formula, data = dd, weights = dd$weight,
                       family = cumulative(parallel = TRUE, reverse = TRUE))
  
  if(model_type == 'polr')
  {
    ctable <- coef(summary(polr_model))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p_value" = p)
    result <- data.frame('coef' = ctable[, "Value"], 'p_value'= ctable[, "p_value"])
    marginal_effects <- margins(polr_model)
  }
  else if (model_type == 'clm')
  {
    ctable <- coef(summary(clm_model))
    result <- data.frame('coef' = ctable[, "Estimate"], 'p_value'= ctable[, "Pr(>|z|)"])
    marginal_effects <- margins(clm_model)
  }
  else
  {
    ctable <- coef(summary(vglm_model))
    result <- data.frame('coef' = ctable[, "Estimate"], 'p_value'= ctable[, "Pr(>|z|)"]) 
    marginal_effects <- margins(polr_model)
  }
    
  result <- round(result,4)
  result <- result %>% mutate(
    p_value = case_when(
      p_value < 0.01 ~ paste(p_value,'**'), 
      p_value < 0.05 ~ paste(p_value,'*'),
      p_value < 0.1 ~ paste(p_value,'~'),
      TRUE ~ as.character(p_value)))
  result$coef_names <- rownames(result)
  
  ## marginal effect
  tmp <- summary(marginal_effects) %>% dplyr:: select(factor, AME)
  names(tmp)[names(tmp) == "factor"] <- "coef_names"
  result <- merge(result,tmp, by = "coef_names", all = TRUE)
  names(result)[-1] <- paste(names(result)[-1],question)
  return (list(result= result, brant = brant_model))
  
}

####........Read the data and clean it############
df <- read_csv("../../Data/clean_file_weighted.csv")
questions <- read.xlsx("../../Data/mappings.xlsx")
perception <- questions[questions$Label %in% c('perception'),]$Question
understanding <- questions[questions$Label %in% c('understanding'), ]$Question
responsibility <- questions[questions$Label %in% c('responsibility'), ]$Question
optimism <- questions[questions$Label %in% c('optimism'), ]$Question 


df <- df %>% mutate( age_aggregated = case_when(
  age %in% c("Under 30","30-39") ~ "Under 40",
  age %in% c("40-49") ~"40-49",
  age %in% c("50-59", "60 years and over")~ "50+",
  age %in% c("Prefer not to say")~NA_character_,
  TRUE~age), 
  age = case_when
  (age =="Prefer not to say"~NA_character_, 
    TRUE~age),
  Trainee = ifelse(reg_status_aggregated=='Trainee',1,0),
  LED_SAS = ifelse(reg_status_aggregated=='LED and SAS',1,0), 
  generetaive_users = ifelse(ai_type_use_qs == 'Generative system', 1,0),
  ddss_users = ifelse(ai_type_use_qs == 'Diagnostic and decision support system', 1,0)
  )

df <- df %>% mutate(medical_area_aggregated = case_when(
  medical_area_aggregated == 'General Practice' ~ 'General_practice',
  medical_area_aggregated == 'Emergency Medicine' ~ 'Emergency_Medicine',
  medical_area_aggregated == 'Anaesthetics and Intensive Care Medicine' ~ 'Anaesthetics',
  TRUE ~ medical_area_aggregated
))

dummy_vars <- model.matrix(~ medical_area_aggregated - 1, data = df)
medical_areas <- colnames(dummy_vars)[-3]
df <- cbind(df, dummy_vars)

df <- df %>%   mutate(across(all_of(c(questions$Question)), 
                             ~ case_when(
                               .%in% c("Strongly agree", "Very optimistic")   ~ 1,
                               .%in% c("Agree", "Somewhat optimistic")  ~ 1,
                               .%in% c("Neutral", "Neither agree nor disagree")  ~ 0,
                               .%in% c("Disagree", "Somewhat pessimistic")  ~ -1,
                               .%in% c("Strongly disagree", "Very pessimistic")  ~ -1,
                               TRUE ~ 0
                             )))

df$ai_type_use_qs <- ifelse(df$ai_type_use_qs %in% 'System efficiency systems', 'Other system',df$ai_type_use_qs)
df$age_aggregated <- relevel(factor(df$age_aggregated, levels = c("Under 40", "40-49","50+")), ref = "Under 40")
df$medical_area_aggregated <- ifelse(is.na(df$medical_area_aggregated),'others',df$medical_area_aggregated)
df$medical_area_aggregated <- relevel(factor(df$medical_area_aggregated, levels = unique(df$medical_area_aggregated)), ref = "General_practice")


df$used_any_ai <- ifelse(df$used_any_ai %in% 'Yes', 1,0)
df$used_gen_ai <- ifelse(df$used_gen_ai %in% 'Yes', 1,0)
df$used_ddss_ai <- ifelse(df$used_ddss_ai %in% 'Yes', 1,0)
df$country_of_practice <- ifelse(is.na(df$country_of_practice), 'other',df$country_of_practice)


df[questions$Question] <- lapply(df[questions$Question], factor)
df[questions$Question] <- lapply(df[questions$Question], ordered)

### Understanding ######
vars <- c('gender', 'age_aggregated', 'generetaive_users',  'pmq', 'Trainee', 'LED_SAS', medical_areas)
all_result <- data.frame()
dd <- df %>% filter(used_any_ai==1)

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}
s <- with(dd, summary(formula, fun=sf))
ss <- matrix(0, nrow = 2, ncol = 5)
for (i in 3:4){
  k <- 1
  for (j in c(2,4,5,7,8)){
    ss[(i-2),k] <- s[,i][[j]]- s[,i][[(j-1)]]
    k <- k+1
  }
}
s[,3][[2]] -  s[,3][[1]]
s[,4][[2]] -  s[,4][[1]]


for (i in 1:8)
{
  question <- understanding[i]
  predictors <- vars[vars != "pmq"]
  if (i %in% 6:8)
   predictors <- vars[!vars %in% c("medical_area_aggregatedRadiology", "generetaive_users", "pmq")]
  formula <- as.formula(paste(question, "~", paste(predictors, collapse = "+")))
  output <- run_ordered_logit(dd, formula, model_type = 'vglm', question)
  brant <- output$brant
  result <- output$result
  colnames(brant)[2:4] <- paste(colnames(brant)[2:4], understanding[i])
  if (i==1)
  {
    brant_all <- brant
    all_result <- result
  }
  else
  {
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
    brant_all <- merge(brant_all,brant, by = "V1", all = TRUE)
  }
}
survey_design <- svydesign(
    ids = ~1,
    weights = ~ weight, # Survey weights variable - I should check this to see
    data = dd
  )
model <- svyolr(formula, design=survey_design)
plot(model$residuals)
waldtest(model)

polr_model <- polr(formula, data = dd, weights = dd$weight, Hess = TRUE)
plot(polr_model$residuals)
waldtest(polr_model)
brant(polr_model)
clm_model <- clm(formula, data = dd, weights = dd$weight)

write.xlsx(all_result, "../../Data/understanding.xlsx")
OUT <- createWorkbook()
addWorksheet(OUT, "vglm")
addWorksheet(OUT, "polr")
addWorksheet(OUT, "clm")

writeData(OUT, sheet = "vglm", x = all_result)
writeData(OUT, sheet = "polr", x = all_result)
writeData(OUT, sheet = "clm", x = all_result)

saveWorkbook(OUT,  "../../Data/understanding.xlsx")

#### Perception #####
vars <- c('gender', 'age_aggregated', 'pmq', 'used_any_ai', 'reg_status_aggregated')
all_result <- data.frame()
for (i in 1:6)
{
  question <- perception[i]
  predictors <- vars
  formula <- as.formula(paste(question, "~", paste(predictors, collapse = "+")))
  dd <- df 
  result <- run_ordered_logit(dd, formula, model_type = 'polr', question = question)
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
write.xlsx(all_result,  "../../Data/perception.xlsx")


#### Responsibility #####
vars <- c('gender', 'age_aggregated', 'pmq', 'used_any_ai', 'reg_status_aggregated')
responsibility <- c(responsibility, optimism)
all_result <- data.frame()

question <- responsibility[i]
predictors <- vars
formula <- as.formula(aiuse_impact_3 ~ gender + age_aggregated + pmq + reg_status_aggregated + ai_type_use_qs)
formula <- as.formula(paste(question, "~", paste(predictors, collapse = "+")))
dd <- df %>% filter(used_any_ai==1)
result <- run_ordered_logit(dd, formula, model_type = 'polr', question = question)
a <- result$result

for (i in 3)
{
  question <- responsibility[i]
  predictors <- vars
  formula <- as.formula(paste(question, "~", paste(predictors, collapse = "+")))
  dd <- df 
  result <- run_ordered_logit(dd, formula, model_type = 'polr', question = question)
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
write.xlsx(all_result,  "../../Data/responsibility.xlsx")

### Checks
for (i in 1:8)
{
  formula <- as.formula(paste(understanding[i], "~", paste(vars, collapse = "+")))
  model <- polr(formula, data = dd, Hess = TRUE, weights = dd$weight)
  print(vif(model))
  print(i)
  print(understanding[i])
  brant_test <- brant(model)
  original <- vglm(formula, data = dd, weights = dd$weight, family = cumulative(parallel = TRUE))
  alternative <- vglm(formula, data = dd, weights = dd$weight,
                      family = cumulative(parallel = FALSE ~ 1  + medical_area_aggregatedRadiology))
  print(lrtest(original, alternative))
}
