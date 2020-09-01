## ISSP MISSING

# Til præsentation 15/9-2020.
# Data skal kunne undersøge mønstre i, hvem der ikke svarer.
# Krav til data:
#   - Hver observation er et svar
#   - Variable identificerer spørgsmålet, respondet, køn, land, type af spørgsmål samt hvorvidt det er missing

library(haven)
library(tidyverse)
library(forcats)
library(questionr)
library(margins)

data_path <- "./data/"
out_path <- "./output/"

setwd(data_path)
issp2017 <- read_dta("ZA6980_v2-0-0.dta")
colnames(issp2017) <- str_replace(colnames(issp2017), "^v(?=\\d{1}$)", "v0")

#vector for all variable names of survey questions
surveyvars <- na.omit(str_extract(colnames(issp2017), "^v\\d{1,2}"))

#vector for variables using 98 and 99 for missing values
vars_mis9899 <- c("v36", "v37", "v44")

#vector for variables using only 99 for missing values
vars_noans99 <- c("v48", "v49", "v50", "v51", "v52", "v65", "v66", "v67")

#vector for variables using only 9 for missing values
vars_noans9 <- c("v60")

#vector for variables using 8 and 9 for missing values
vars_mis89 <- surveyvars[!(surveyvars %in% c(vars_mis9899, vars_noans99, vars_noans9))]

#subsetting data to include only studyno, caseid, country and survey questions
issp_subset <- issp2017 %>%
  select("CASEID", "studyno", "country", all_of(surveyvars)) %>%
  mutate(country = as_factor(country))

#long mutation
issp_long <- pivot_longer(issp_subset, cols = starts_with("v"), names_to = "variable", values_to = "response") %>%
  mutate(missing = as.numeric(case_when(variable %in% vars_mis9899 & response == 99 ~ "1",
                             variable %in% vars_mis9899 & response == 98 ~ "1",
                             variable %in% vars_noans99 & response == 99 ~ "1",
                             variable %in% vars_noans9 & response == 9 ~ "1",
                             variable %in% vars_mis89 & response == 8 ~ "1",
                             variable %in% vars_mis89 & response == 9 ~ "1",
                             TRUE ~ "0")),
         year = 2017) %>%
  arrange(desc(missing))

#add background info
issp_long <- left_join(issp_long, select(issp2017, CASEID, URBRURAL, MARITAL, SEX, BIRTH, EDUCYRS), by = "CASEID")

#fix classes
issp_long <- issp_long %>%
  mutate(age = 2017 - as.numeric(BIRTH),
         age = na_if(age, as.numeric(BIRTH) > 2017),
         SEX = as.factor(SEX)) %>%
  mutate(SEX = fct_drop(na_if(SEX, 9)))

#labels
library(labelled)
library(purrr)
issp2017_labels <- var_label(issp2017)
issp2017_labelsdf <- data.frame(variable = names(issp2017_labels), label = sapply(issp2017_labels, "[[", 1), stringsAsFactors = FALSE)


#export variable list
issp_variables <- issp_long %>%
  left_join(issp2017_labelsdf, by = "variable") %>%
  group_by(variable, label) %>%
  summarise(type = "")

library(xlsx)
write.xlsx(as.data.frame(issp_variables), "../output/issp2017_variables.xlsx", row.names = FALSE)

# logit
model <- glm(missing ~ SEX + age + country, family = binomial(link = "logit"), data = issp_long)
exp(coef(model))

summary(margins(model, data = subset(issp_long, country == "DK-Denmark"), 
                at = list(age = c(20, 30, 40, 50, 60)), variables = "SEX"))

#functions for counting missing values
count_undec8 <- function(var){
  var_count_undec = sum(var ==8)
  return(var_count_undec)
}

count_undec98 <- function(var){
  var_count_undec = sum(var ==98)
  return(var_count_undec)
}

count_norep9 <- function(var){
  var_count_norep = sum(var ==9)
  return(var_count_norep)
}

count_norep99 <- function(var){
  var_count_norep = sum(var ==99)
  return(var_count_norep)
}