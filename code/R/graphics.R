#Library imports
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(readxl)
library(tidyr)
library(openxlsx)
library(scales)
library(gridExtra)
library(survey)
library("gridExtra")
library(writexl)
library(dplyr)
library(round)
library(forcats)


### Function Definition #######
common_theme <- theme( 
  plot.caption= element_text(size=14, hjust = 0.5, vjust = 0, family="serif") , 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  axis.text.y = element_text(size = 16, face = "bold"), 
  plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face="bold"),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 12), 
  plot.subtitle =  element_text(hjust = 0.5, vjust = 1, size = 14, face="bold"), 
  legend.position = "top"
  )
custom_colors <- c("#EEAA33", "#AA99EE", "#99EEAA")

#function to make sure groups of % dont accidentally 
#sum to more or less than 100 with rounding
round_preserve_sum <- function(x, target_sum) {
  rounded <- floor(x)
  diff <- target_sum - sum(rounded)
  residuals <- x - rounded
  adjustment_indices <- order(residuals, decreasing = TRUE)[1:diff]
  rounded[adjustment_indices] <- rounded[adjustment_indices] + 1
  return(rounded)
}

#### plot understanding questions - Generative versus DDSS ######
df_understanding <- read_csv("../../Data/understanding.csv")#seems to load some regression coefficients
questions <- read_xlsx("../../Data/mappings.xlsx")
colnames(questions)[names(questions)=='Question'] <- 'variable'
df_understanding <- merge(df_understanding, questions, by = c('variable'))

df_understanding <- df_understanding %>% group_by(variable) %>% mutate(freq_weighted = round_preserve_sum(freq_weighted,100))
df_understanding <- df_understanding %>% group_by(variable) %>% mutate(freq_weighted_dss = round_preserve_sum(freq_weighted_dss,100))
df_understanding <- df_understanding %>% group_by(variable) %>% mutate(freq_weighted_gen = round_preserve_sum(freq_weighted_gen,100))


p<- ggplot(df_understanding, aes(x= str_wrap(Text, width = 50), y=as.double(freq_weighted), fill = reorder(value, rep(c(3,1,2),nrow(df_understanding)/3)))) +
  geom_bar(position = 'stack', stat='identity', color='black', width=0.3) +
  scale_fill_manual(values = custom_colors,name = "") + coord_flip() + 
  geom_text(aes(label = paste0(round(as.double(freq_weighted)), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color ="black") +  labs(title="All AI users") + 
  common_theme
p
ggsave("../../Figures/understanding_all.jpg", plot=p)

p1<- ggplot(df_understanding, aes(x= variable, y=as.double(freq_weighted_dss), fill = reorder(value, rep(c(3,1,2),nrow(df_understanding)/3)))) +
  geom_bar(position = 'stack', stat='identity', color='black', width=0.3) +
  scale_fill_manual(values = custom_colors,name = "") + coord_flip() + 
  geom_text(aes(label = paste0(round(as.double(freq_weighted_dss)), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, color ="black") +  labs(title="Diagnostic and Decision Support") + 
  common_theme
p1
p2 <- ggplot(df_understanding, aes(x= variable, y= as.double(freq_weighted_gen), fill = reorder(value, rep(c(3,1,2),nrow(df_understanding)/3)))) +
  geom_bar(position = 'stack', stat='identity', color='black', width=0.3) +
  scale_fill_manual(values = custom_colors, name = "") + coord_flip() + 
  geom_text(aes(label = paste0(round(as.double(freq_weighted_gen)), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, color ="black") + labs(title="Generative AI") + 
  common_theme + theme(legend.position = "top")
a <- grid.arrange(p1, p2,  ncol = 2) 
ggsave("../../Figures/understanding.jpg", plot=a, h = 6, w = 12)


########## Perception Questions ##############################
df_perception <- read_excel("../../Data/perception.xlsx")
df_perception <- df_perception %>% group_by(variable) %>% mutate(freq_weighted = round_preserve_sum(freq_weighted,100))
df_perception <- df_perception %>% group_by(variable) %>% mutate(freq_weighted_ai = round_preserve_sum(freq_weighted_ai,100))
df_perception <- df_perception %>% group_by(variable) %>% mutate(freq_weighted_no_ai = round_preserve_sum(freq_weighted_no_ai,100))

questions <- read_xlsx("../../Data/table_statistics.xlsx", sheet = 'Glossary')
colnames(questions)[names(questions)=='Question'] <- 'variable'
df_perception <- merge(df_perception, questions, by = c('variable'))

#### plot Responsibility
df_responsibility <- df_perception %>% filter(grepl("aiuse_perception", variable))
l <- ggplot(df_responsibility, aes(x= str_wrap(Text),  y =freq_weighted, fill = reorder(value, rep(c(3,1,2),nrow(df_responsibility)/3)))) + 
  geom_bar(position="stack", stat="identity", width = 0.4) + 
  scale_fill_manual(values = custom_colors, name = "")  + coord_flip() +
  geom_text(aes(label = paste0(round(freq_weighted), "%")), 
            position = position_stack(vjust = 0.5), size = 7, color ="black") + common_theme
l
ggsave("../../Figures/responsibility.jpg", plot=p)


p1 <- ggplot(df_responsibility, aes(x= str_wrap(Text, width = 30), y=as.double(freq_weighted_ai), fill = reorder(value, rep(c(3,1,2),nrow(df_responsibility)/3)))) +
  geom_bar(position = 'stack', stat='identity', color='black', width=0.3) +
  scale_fill_manual(values = custom_colors,name = "") + coord_flip() + 
  geom_text(aes(label = paste0(round(as.double(freq_weighted_ai)), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color ="black") +  labs(title="AI users") + 
  common_theme

p2 <- ggplot(df_responsibility, aes(x= str_wrap(Text, width = 30), y= as.double(freq_weighted_no_ai), fill = reorder(value, rep(c(3,1,2),nrow(df_responsibility)/3)))) +
  geom_bar(position = 'stack', stat='identity', color='black', width=0.3) +
  scale_fill_manual(values = custom_colors, name = "") + coord_flip() + 
  geom_text(aes(label = paste0(round(as.double(freq_weighted_no_ai)), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color ="black") + labs(title="Non AI users") + 
  common_theme + theme(legend.position = "top")
a <- grid.arrange(p1, p2,  ncol = 2)
ggsave("../../Figures/responsibility_ai_no.jpg", plot=a)



### AI impacts. 
df_perception[df_perception$value=="Pessimistic" ,'value'] <- "Disagree"
df_perception[df_perception$value=="Optimistic" ,'value'] <- "Agree"
df_impact <- df_perception %>% filter(grepl("aiuse_impact", variable) | grepl("aiuse_optimism", variable))

p <- ggplot(df_impact, aes(x= str_wrap(Text, width = 30),  y =freq_weighted, fill = reorder(value, rep(c(3,1,2),nrow(df_impact)/3)))) + 
  geom_bar(position="stack", stat="identity", width = 0.4) + 
  scale_fill_manual(values = custom_colors, name = "")  + coord_flip() +
  geom_text(aes(label = paste0(round(freq_weighted), "%")), 
            position = position_stack(vjust = 0.5), size = 7, color ="black") + common_theme

p
ggsave("../../Figures/impact.jpg", plot=p, h = 6, w = 12)

p1 <- ggplot(df_impact, aes(x= str_wrap(Text, width = 30), y=as.double(freq_weighted_ai), fill = reorder(value, rep(c(3,1,2),nrow(df_impact)/3)))) +
  geom_bar(position = 'stack', stat='identity', color='black', width=0.3) +
  scale_fill_manual(values = custom_colors,name = "") + coord_flip() + 
  geom_text(aes(label = paste0(round(as.double(freq_weighted_ai)), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color ="black") +  labs(title="AI users") + 
  common_theme

p2 <- ggplot(df_impact, aes(x= str_wrap(Text, width = 30), y= as.double(freq_weighted_no_ai), fill = reorder(value, rep(c(3,1,2),nrow(df_impact)/3)))) +
  geom_bar(position = 'stack', stat='identity', color='black', width=0.3) +
  scale_fill_manual(values = custom_colors, name = "") + coord_flip() + 
  geom_text(aes(label = paste0(round(as.double(freq_weighted_no_ai)), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color ="black") + labs(title="Non AI users") + 
  common_theme + theme(legend.position = "top")

a <- grid.arrange(p1, p2,  ncol = 2)
ggsave("../../Figures/impact_ai_no.jpg", plot=a)

####################### Correlation ##################
df <- read_csv("../../Data/clean_file_weighted.csv")
questions <- read_xlsx("../../Data/mappings.xlsx")
perception <- questions[questions$Label %in% c('perception'),]$Question
understanding <- questions[questions$Label %in% c('understanding'), ]$Question
responsibility <- questions[questions$Label %in% c('responsibility'), ]$Question
optimism <- questions[questions$Label %in% c('optimism'), ]$Question 
dd <- df %>%   mutate(across(all_of(c(questions$Question)), 
                             ~ case_when(
                               .%in% c("Strongly agree", "Very optimistic")   ~ 2,
                               .%in% c("Agree", "Somewhat optimistic")  ~ 1,
                               .%in% c("Neutral", "Neither agree nor disagree")  ~ 0,
                               .%in% c("Disagree", "Somewhat pessimistic")  ~ -1,
                               .%in% c("Strongly disagree", "Very pessimistic")  ~ -2,
                               TRUE ~ 0
                             )))


da <- dd %>% filter(used_any_ai %in% 'Yes')
col_names <- c(perception,optimism,understanding, responsibility)
custom_labels <- questions[questions$Question %in% col_names, 'Text']
p <- da  %>% dplyr:: select(all_of(col_names)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, 
                         lab_size=6, method = "square") + common_theme + 
  theme(axis.text.x = element_text(angle = 20, size = 15, face = 'bold'),
        axis.text.y = element_text(size = 15, face = 'bold')) + labs(title="Correlations between all questions")
p
ggsave("../../Figures/all_correlations.jpg", plot=p)

col_names <- c(perception[4:6],optimism)
custom_labels <- questions[questions$Question %in% col_names, 'Text']
p <- dd  %>% dplyr:: select(all_of(col_names)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, 
                         lab_size=6, method = "square") + 
  theme(axis.text.x = element_text(angle = 20, size = 24, face = 'bold'),
        axis.text.y = element_text(size = 24, face = 'bold'))
p
ggsave("../../Figures/perception_high_correlation.jpg", plot=p)

col_names <- c(responsibility)
custom_labels <- questions[questions$Question %in% col_names, 'Text']$Text
p <- dd %>% dplyr:: select(all_of(col_names)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, 
                         lab_size=8, method = "square") + 
  theme(axis.text.x = element_text(angle = 20, size = 24, face = 'bold'),
        axis.text.y = element_text(size = 24, face = 'bold'))
p
ggsave("../../Figures/responsibility.jpg", plot=p)

# sort out the labels, write the summary of result back 
col_names <- c(perception[4],optimism, understanding[-c(1,2,6)])
custom_labels <- questions[questions$Question %in% col_names,]$Text
corr <- dd %>% filter(used_any_ai == "Yes") %>% dplyr:: select(all_of(col_names)) %>% 
  cor(use="pairwise.complete.obs")
colnames(corr) 
p <- corr %>% ggcorrplot::ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, 
                         lab_size=8, method = "square") + 
  theme(axis.text.x = element_text(angle = 20, size = 20, face = 'bold'),
        axis.text.y = element_text(size = 20, face = 'bold'),  
        legend.title = element_text(size = 18),  # Title size
        legend.text = element_text(size = 18))
p
ggsave("../../Figures/high_correlation.jpg", plot=p)

temp <- dd %>% filter(used_any_ai %in% 'Yes')
corr_training_concerns <- cor(temp$ai_use_training,temp$ai_use_concerns)
model <- lm(temp$ai_use_training ~ temp$ai_use_concerns)
summary(model)

corr_training_explain <- cor(temp$ai_use_training,temp$ai_use_explain)
model <- lm(temp$ai_use_training ~ temp$ai_use_explain)
summary(model)



#Reordered graphics JB 08/2024----

####Graphic 1 ----
df_perception[df_perception$value=="Pessimistic" ,'value'] <- "Disagree"
df_perception[df_perception$value=="Optimistic" ,'value'] <- "Agree"
df_perception$value2 <- fct_relevel(as.factor(df_perception$value), 'Disagree', 'Neutral', 'Agree')

#variables for graphics
graphic_1.1_vars <- data.frame(
  Text = c('I am optimistic about the integration of AI systems in healthcare/clinical practice', 'Opportunities for AI in healthcare are being fully explored', 'AI is being deployed before it is ready in my area of practice'),
  textlabel = c('I am optimistic about the integration of AI systems\nin healthcare/clinical practice', 'Opportunities for AI in healthcare\nare being fully explored', 'AI is being deployed before it is ready\nin my area of practice')
)

graphic_1.2_vars <- data.frame(
  Text = c('Advances in AI are making me worried about my job security', 'Advances in AI are likely to limit training or learning opportunities'),
  textlabel = c('Advances in AI are making me\nworried about my job security', 'Advances in AI are likely to limit\ntraining or learning opportunities')
)

df_g11 <- df_perception %>%
  inner_join(graphic_1.1_vars, 'Text') %>%
  select(Text, value2, freq_weighted_no_ai, freq_weighted, freq_weighted_ai, textlabel) %>%
  pivot_longer(!c('Text', 'value2', 'textlabel'), names_to = "freqtype", values_to = "freqweighted") %>%
  mutate(
  freqtype2 = case_when(
      freqtype == 'freq_weighted_no_ai' ~ 'AI non-user',
      freqtype == 'freq_weighted_ai' ~ 'AI user',
      freqtype == 'freq_weighted' ~ 'All respondents',
      )
  ) %>%
  mutate (
    freqtype2 = fct_relevel(as.factor(freqtype2), 'All respondents', 'AI user', 'AI non-user')
  )


ggplot(df_g11, aes(x=freqtype2, y=freqweighted/100, fill=value2)) +
  geom_bar(stat='identity', position='stack', alpha=0.8, color='black', width = 0.7) +
  facet_wrap(~textlabel, ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neutral', 'Disagree')) + xlab('') + ylab('') +
  theme_minimal() +   theme(strip.text = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freqweighted), "%")), position = position_stack(vjust = 0.5), size = 6, color ="black")


df_g12 <- df_perception %>%
  inner_join(graphic_1.2_vars, 'Text') %>%
  select(Text, value2, freq_weighted_no_ai, freq_weighted, freq_weighted_ai, textlabel) %>%
  pivot_longer(!c('Text', 'value2', 'textlabel'), names_to = "freqtype", values_to = "freqweighted") %>%
  mutate(
    freqtype2 = case_when(
      freqtype == 'freq_weighted_no_ai' ~ 'AI non-user',
      freqtype == 'freq_weighted_ai' ~ 'AI user',
      freqtype == 'freq_weighted' ~ 'All respondents',
    )
  ) %>%
  mutate (
    freqtype2 = fct_relevel(as.factor(freqtype2), 'All respondents', 'AI user', 'AI non-user')
  )

ggplot(df_g12, aes(x=freqtype2, y=freqweighted/100, fill=value2)) +
  geom_bar(stat='identity', position='stack', alpha=0.8, color='black') +
  facet_wrap(~textlabel, ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neutral', 'Disagree')) + xlab('') + ylab('') +
  theme_minimal() + theme(strip.text = element_text(size = 10, face = 'bold'), axis.text.x = element_text(size = 10)) +  
  geom_text(aes(label = paste0(round(freqweighted), "%")), position = position_stack(vjust = 0.5), size = 5, color ="black")

####Graphic 2 ----

graphic_2.1_vars <- data.frame(
  Text = c('I understand the risks of AI in healthcare in my area of practice', 'I understand who is responsible if a decision is made incorrectly involving an AI system', 'I have had sufficient training to understand my professional responsibilities when using AI systems'),
  textlabel = c('I understand the risks of AI in healthcare\nin my area of practice', 'I understand who is responsible if a decision\nis made incorrectly involving an AI system', 'I have had sufficient training to understand\nmy professional responsibilities\nwhen using AI systems')
)

graphic_2.2_vars <- data.frame(
  Text = c('Advances in AI are likely to erode my professional autonomy', 'I would feel confident to ignore the recommendations of an AI system within my area of practice'),
  textlabel = c('Advances in AI are likely to\nerode my professional autonomy', 'I would feel confident to ignore\nthe recommendations of an AI system within my area of practice')
)


df_g21 <- df_perception %>%
  inner_join(graphic_2.1_vars, 'Text') %>%
  select(Text, value2, freq_weighted_no_ai, freq_weighted, freq_weighted_ai, textlabel) %>%
  pivot_longer(!c('Text', 'value2', 'textlabel'), names_to = "freqtype", values_to = "freqweighted") %>%
  mutate(
    freqtype2 = case_when(
      freqtype == 'freq_weighted_no_ai' ~ 'AI non-user',
      freqtype == 'freq_weighted_ai' ~ 'AI user',
      freqtype == 'freq_weighted' ~ 'All respondents',
    )
  ) %>%
  mutate (
    freqtype2 = fct_relevel(as.factor(freqtype2), 'All respondents', 'AI user', 'AI non-user')
  )

ggplot(df_g21, alpha=0.5, aes(x=freqtype2, y=freqweighted/100, fill=value2)) +
  geom_bar(stat='identity', position='stack', alpha=0.8, color='black', width = 0.8) +
  facet_wrap(~textlabel, ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neutral', 'Disagree')) + xlab('') + ylab('') +
  theme_minimal() +  theme(strip.text = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freqweighted), "%")), position = position_stack(vjust = 0.5), size = 6, color ="black")



df_g22 <- df_perception %>%
  inner_join(graphic_2.2_vars, 'Text') %>%
  select(Text, value2, freq_weighted_no_ai, freq_weighted, freq_weighted_ai, textlabel) %>%
  pivot_longer(!c('Text', 'value2', 'textlabel'), names_to = "freqtype", values_to = "freqweighted") %>%
  mutate(
    freqtype2 = case_when(
      freqtype == 'freq_weighted_no_ai' ~ 'AI non-user',
      freqtype == 'freq_weighted_ai' ~ 'AI user',
      freqtype == 'freq_weighted' ~ 'All respondents',
    )
  ) %>%
  mutate (
    freqtype2 = fct_relevel(as.factor(freqtype2), 'All respondents', 'AI user', 'AI non-user')
  )

ggplot(df_g22, alpha=0.5, aes(x=freqtype2, y=freqweighted/100, fill=value2)) +
  geom_bar(stat='identity', position='stack', alpha=0.8, color='black', width = 0.7) +
  facet_wrap(~textlabel, ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neutral', 'Disagree')) + xlab('') + ylab('') +
  theme_minimal() + theme(strip.text = element_text(size = 10, face = 'bold'), axis.text.x = element_text(size = 10)) +
  geom_text(aes(label = paste0(round(freqweighted), "%")), position = position_stack(vjust = 0.5), size = 5, color ="black")


####Graphic 3 ----

graphic_3.1_vars <- data.frame(
  Text = c('I feel confident using the system', 'I was consulted during the deployment or integration of the AI system', 'The system improves my clinical decision making', 'The system has increased my productivity'),
  textlabel = c('I feel confident using the system', 'I was consulted during the deployment\nor integration of the AI system', 'The system improves my clinical decision making', 'The system has increased my productivity')
)

df_understanding$value2 <- fct_relevel(as.factor(df_understanding$value), 'Disagree', 'Neutral', 'Agree')

df_g31 <- df_understanding %>% ungroup()  %>%
  inner_join(graphic_3.1_vars, 'Text') %>%
  select(Text, value2, freq_weighted_gen, freq_weighted_dss, textlabel) %>%
  pivot_longer(!c('Text', 'value2', 'textlabel'), names_to = "freqtype", values_to = "freqweighted") %>%
  mutate(
    freqtype2 = case_when(
  freqtype == 'freq_weighted_gen' ~ 'Generative AI',
  freqtype == 'freq_weighted_dss' ~ 'Diagnostic and decision support'
  )
)

ggplot(df_g31, aes(x=freqtype2, y=freqweighted/100, fill=value2)) +
  geom_bar(stat='identity', position='stack', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~textlabel, ncol=2) + 
  scale_y_continuous(labels=scales::percent_format()) +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neutral', 'Disagree')) + xlab('') + ylab('') +
  theme_minimal() + theme(strip.text = element_text(size = 12, face = 'bold'), axis.text.x = element_text(size = 10)) +
  geom_text(aes(label = paste0(round(freqweighted), "%")), position = position_stack(vjust = 0.5), size = 5, color ="black")


graphic_3.2_vars <- data.frame(
  Text = c('The outputs of the system are clear and understandable', 'I understand how to raise any concerns I have about the system', 'If appropriate, I could explain the outputs of the system to patients', 'I have received sufficient training on the system'),
  textlabel = c('The outputs of the system are\nclear and understandable', 'I understand how to raise any concerns\nI have about the system', 'If appropriate, I could explain\nthe outputs of the system to patients', 'I have received sufficient training\non the system')
)


df_g32 <- df_understanding %>% ungroup()  %>%
  inner_join(graphic_3.2_vars, 'Text') %>%
  select(Text, value2, freq_weighted_gen, freq_weighted_dss, textlabel) %>%
  pivot_longer(!c('Text', 'value2', 'textlabel'), names_to = "freqtype", values_to = "freqweighted") %>%
  mutate(
    freqtype2 = case_when(
      freqtype == 'freq_weighted_gen' ~ 'Generative AI',
      freqtype == 'freq_weighted_dss' ~ 'Diagnostic and decision support'
    )
  )

ggplot(df_g32, aes(x=freqtype2, y=freqweighted/100, fill=value2)) +
  geom_bar(stat='identity', position='stack', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~textlabel, ncol=2) + 
  scale_y_continuous(labels=scales::percent_format()) +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neutral', 'Disagree')) + xlab('') + ylab('') +
  theme_minimal() +   theme_minimal() + theme(strip.text = element_text(size = 12, face = 'bold'), axis.text.x = element_text(size = 10)) +
  geom_text(aes(label = paste0(round(freqweighted), "%")), position = position_stack(vjust = 0.5), size = 5, color ="black")


#Scenario question statistics ----

#Num doctors responding
tot_responses <- nrow(read_xlsx("../../Data/Scenario_question_analysis (version 1).xlsx"))
df_scenario_all <- read_xlsx("../../Data/Scenario_question_analysis (version 1).xlsx") %>% 
  select(all_of(response_labels$response)) %>%  mutate_all(~ coalesce(., 0)) 


# Make column for those who mentioned ignoring without mentioning any other thing. 
cols_to_check <- setdiff(names(df_scenario_all), 'Ignore/proceed with own clincial judgement')
df_scenario_all$ignore <- ifelse(
  rowSums(df_scenario_all[, cols_to_check] == 0) == length(cols_to_check) & df_scenario_all$`Ignore/proceed with own clincial judgement` == 1, 
  1, 
  0
)

#there were 929 total survey responses
tot_responses/929


response_labels <- 
  data.frame (response = c("Would document reasons/capture audit trial/rerun, check and review AI data, if possible", "Ignore/proceed with own clincial judgement","ignore",
                           "Follow AI", "Discuss with colleagues/senior consultant/MDT", "Discuss with patient/involve patient", "Follow up with AI developer, data team or other third party/flag alarm/raise concerns", "Double check my own reasoning/question myself/establish my reasons to disagree/cause concern", "Check internal or external guidelines/research/look for trial or assurance guidance/research literature"),
              labels = c("Check data and rerun system", "Proceed with my own clincial judgement", "Ignore without further actions", "Follow the AI judgment", "Discuss with colleagues", "Discuss with patient", "Follow up with AI developer and raise concerns", "Double check my own reasoning and question myself", "Check internal or external guidelines or research literature")
  )
  
df_scenario <- 
  df_scenario_all %>%
  select(all_of(response_labels$response)) %>%
  colSums(na.rm=T) %>%
  data.frame('response' = names(.), value=., row.names = NULL) %>%
  mutate(prop = value/tot_responses) %>% #as prop of total people who gave an answer
  left_join(response_labels, by='response')

  
ggplot(df_scenario, alpha=0.5, aes(x=reorder(labels, prop), y=prop)) +
  geom_bar(stat='identity', alpha=0.8, color='black') +
  scale_y_continuous('', labels=scales::percent_format()) +
  xlab('') + 
  theme_minimal() +   geom_text(aes(label = paste0(round(prop*100), "%")), position = position_stack(vjust = 0.9), size = 5, color ="black") + 

  coord_flip() +   theme(axis.text.y = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 14))



