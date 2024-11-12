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
#library(ANOVA)

### Function definitions
run_logit <- function(df, vars, option = 'normal'){
  AI_type <- c('used_any_ai','used_gen_ai','used_ddss_ai')
  summary_all <- data.frame()
  vif_all <- data.frame()
  for (i in 1:length(AI_type))
  {
    formula <- as.formula(paste(AI_type[i], "~", paste(vars, collapse = "+"))) 
    if (option=='survey'){
      survey_design <- svydesign(
        ids = ~1,
        weights = ~ weight, # Survey weights variable - I should check this to see
        data = df
      )
      model <- svyglm(formula, design=survey_design, family= quasibinomial())
    }
    else
      model <- glm(formula, data = df , family = quasibinomial(), weights = df$weight)
    print(AI_type[i])
    s <- summary(model)
    marginal_effects <- margins(model,design=survey_design )
    
    ### Hosmer-Lemeshow test - 
    #High p-value (> 0.05): Indicates that the model fits the data well. 
    #The null hypothesis (that the observed and expected frequencies are similar) is not rejected, 
    #suggesting no evidence of lack of fit.
    fit <- model$fitted
    hoslem_test <- hoslem.test(df[[AI_type[i]]], fit, g = 10)
    print(hoslem_test)
    
    roc_curve <- roc(df[[AI_type[i]]], fit)
    auc_value <- auc(roc_curve)
    
    ##High p-value (> 0.05) The null hypothesis (that the observed and expected frequencies are similar) is not rejected, 
    #suggesting no evidence of lack of fit. - This is more straightforward version of hoslem test
    r <- (df[[AI_type[i]]] - fit)/(sqrt(fit*(1-fit)))
    p <- 1- pchisq(sum(r^2), df = 929 - s$df[1]-1)
    print(paste("Pschisq_1",p))
    
    #### Deviance and Likelihood Ratio Test - 
    # A low p-value (typically < 0.05) indicates that the predictors significantly improve the model fit 
    # which means the null hypothesis is rejected, suggesting significant difference between model and null model
    null_model <- glm(as.formula(paste(AI_type[i], "~ 1")), data = df,  family =quasibinomial(), weights = df$weight)
    anov <- anova(null_model, model, test = "Chisq")
    print(anov)
    
    
    ### difference between model and null model - higher is better, greater than zero, p_value < 0.05 - like anova 
    print("Pschisq_2:") ## tests
    print(1- pchisq(s$null.deviance-s$deviance, df= s$df[1]))
    
    print(vif(model))
    
    # Pseudo R-squared
    R2_McFadden <- 1 - (model$deviance / model$null.deviance)
    print(paste("R2_McFadde:", R2_McFadden))
    
    R2_CoxSnell <- 1 - exp(( model$null.deviance - model$deviance) / 929)
    print(paste("R2_CoxSnell:", R2_CoxSnell))
    
    R2_Nagelkerke <- R2_CoxSnell / (1 - exp(model$null.deviance/929))
    print(paste("R2_Nagelkerke:", R2_Nagelkerke))
    
    summary <- as.data.frame(summary(model)$coefficients)
    names(summary)[names(summary) == "Pr(>|t|)" | names(summary) == "Pr(>|z|)"] <- "p_value"
    summary$coef_names <- rownames(summary)
    tmp <- summary(marginal_effects) %>% dplyr:: select(factor, AME)
    names(tmp)[names(tmp) == "factor"] <- "coef_names"
    summary <- merge(summary,tmp, by = "coef_names")
    summary <- summary[c('coef_names', 'Estimate','p_value', 'AME')]
    
    summary <- rbind(summary,c('R2_McFadden',as.double(R2_McFadden),'',''))
    summary <- rbind(summary,c('AUC',as.double(auc_value),'',''))
    summary <- rbind(summary, c('hoslem_test',hoslem_test$statistic, as.double(hoslem_test$p.value),''))
    summary <- rbind(summary, c('AIC',AIC(model)[2],'',''))
    summary <- rbind(summary, c('Annova',as.double(anov[2,4]), as.double(anov[2,5]),''))
    summary$p_value <- as.double(summary$p_value)
    summary$Estimate <- as.double(summary$Estimate)
    summary <- summary %>% mutate(
      p_value = case_when(
        p_value < 0.01 ~ paste(round(p_value,4),'**'), 
        p_value < 0.05 ~ paste(round(p_value,4),'*'),
        p_value < 0.1 ~ paste(round(p_value,4),'~'),
      TRUE ~ as.character(round(p_value,4))), 
      Estimate  = round(Estimate,4))
    names(summary)[2:4] <- paste(names(summary)[2:4],AI_type[i])
    if (i==1)
    {
      summary_all <- summary
      vif_all <- vif(model)
    }
    else
    {
      summary_all <- merge(summary_all,summary, by = 'coef_names', all = TRUE)
      vif_all <- cbind(vif_all, vif(model))
    }
  }
  vif_all <- cbind(vars, vif_all)
  return (list(table = summary_all, vif = vif_all, model = model))
}

####........Read the data and clean it############
df <- read_csv("../../Data/clean_file_June_NA.csv")
questions <- read.xlsx("../../Data/mappings.xlsx")
perception <- questions[questions$Label %in% c('perception'),]$Question
understanding <- questions[questions$Label %in% c('understanding'), ]$Question
responsibility <- questions[questions$Label %in% c('responsibility'), ]$Question
optimism <- questions[questions$Label %in% c('optimism'), ]$Question 


df <- df %>% mutate(age_aggregated = case_when(
  age %in% c("Under 30","30-39") ~ "Under 40",
  age %in% c("40-49") ~"40-49",
  age %in% c("50-59", "60 years and over")~ "50+",
  TRUE~age),
  Trainee = ifelse(reg_status_aggregated=='Trainee',1,0),
  LED_SAS = ifelse(reg_status_aggregated=='LED and SAS',1,0), 
  generetaive_users = ifelse(ai_type_use_qs == 'Generative system', 1,0),
  ddss_users = ifelse(ai_type_use_qs == 'Diagnostic and decision support system', 1,0))

df[is.na(df$inside_uk_years_aggregated), 'inside_uk_years_aggregated'] <- "other"

df <- df %>%   mutate(across(all_of(c(questions$Question)), 
                             ~ case_when(
                               .%in% c("Strongly agree", "Very optimistic")   ~ 2,
                               .%in% c("Agree", "Somewhat optimistic")  ~ 1,
                               .%in% c("Neutral", "Neither agree nor disagree")  ~ 0,
                               .%in% c("Disagree", "Somewhat pessimistic")  ~ -1,
                               .%in% c("Strongly disagree", "Very pessimistic")  ~ -2,
                               TRUE ~ 0
                             )))


df$ai_type_use_qs <- ifelse(df$ai_type_use_qs %in% 'System efficiency systems', 'Other system',df$ai_type_use_qs)
df$age_aggregated <- relevel(factor(df$age_aggregated, levels = c("Under 40", "40-49","50+")), ref = "Under 40")
df$medical_area_aggregated <- ifelse(is.na(df$medical_area_aggregated),'others',df$medical_area_aggregated)
df$medical_area_aggregated <- relevel(factor(df$medical_area_aggregated, levels = unique(df$medical_area_aggregated)), ref = "General Practice")


df$used_any_ai <- ifelse(df$used_any_ai %in% 'Yes', 1,0)
df$used_gen_ai <- ifelse(df$used_gen_ai %in% 'Yes', 1,0)
df$used_ddss_ai <- ifelse(df$used_ddss_ai %in% 'Yes', 1,0)
df$country_of_practice <- ifelse(is.na(df$country_of_practice), 'other',df$country_of_practice)



######## Run binomial Logistic Regression for different set of Variables##########
vars_1 <- c('age_aggregated', 'gender', 'pmq', 'medical_area_aggregated', 'LED_SAS', 'Trainee')
vars_2 <- c('age_aggregated', 'gender', 'pmq', 'medical_area_aggregated', 'reg_status_aggregated')
vars_3 <- c('age_aggregated', 'gender', 'pmq', 'medical_area_aggregated', 'reg_status_aggregated', 'patient_facing_role', 'inside_uk_years_aggregated')

output_1 <- run_logit(df, vars_1, 'survey')
output_2 <- run_logit(df, vars_2, 'survey')
output_3 <- run_logit(df, vars_3, 'survey')
t_1 <- output_1$table
t_2 <- output_2$table
t_3 <- output_3$table

OUT <- createWorkbook()
addWorksheet(OUT, "model_1")
addWorksheet(OUT, "model_2")
addWorksheet(OUT, "model_3")
addWorksheet(OUT, "vif_1")
addWorksheet(OUT, "vif_2")
addWorksheet(OUT, "vif_3")

writeData(OUT, sheet = "model_1", x = t_1)
writeData(OUT, sheet = "model_2", x = t_2)
writeData(OUT, sheet = "model_3", x = t_3)
writeData(OUT, sheet = "vif_1", x = output_1$vif)
writeData(OUT, sheet = "vif_2", x = output_2$vif)
writeData(OUT, sheet = "vif_3", x = output_3$vif)
saveWorkbook(OUT,  "../../Data/Use_Regressions.xlsx")
