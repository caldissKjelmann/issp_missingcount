## ISSP MISSING

#Sanne skal fremlægge 5/12 om mønstre i missing: hvilke spørgsmål, bliver ikke besvaret samt lande- og årssammenligning.

#Forsøg med et enkelt modul og lav datastruktur, der tæller missing for hver variabel for hvert land. 
#Script skal laves sådan, at det nemt kan udvides med flere datasæt.

library(haven)
library(tidyverse)

data_path <- 'D:/Data/issp/issp2017'

setwd(data_path)
issp2017 <- read_dta("ZA6980_v2-0-0.dta")

surveyvars <- na.omit(str_extract(colnames(issp2017), "^v\\d{1,2}"))

vars_mis9899 <- c("v36", "v37", "v44")
vars_noans99 <- c("v48", "v49", "v50", "v51", "v52", "v65", "v66")
vars_noans9 <- c("v60")
vars_mis89 <- surveyvars[!(surveyvars %in% c(vars_mis9899, vars_noans99, vars_noans9))]

issp_subset <- issp2017 %>%
  select("studyno", "country", surveyvars) %>%
  mutate(country = as_factor(country))

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

df_count_undec8 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(vars_mis89), funs(count_undec8))
colnames(df_count_undec8)[-c(1,2)] <- paste0(colnames(df_count_undec8)[-c(1,2)], "_count_undec")

df_count_undec98 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(vars_mis9899), funs(count_undec98))
colnames(df_count_undec98)[-c(1,2)] <- paste0(colnames(df_count_undec98)[-c(1,2)], "_count_undec")

df_count_norep9 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(c(vars_mis89, vars_noans9)), funs(count_norep9))
colnames(df_count_norep9)[-c(1,2)] <- paste0(colnames(df_count_norep9)[-c(1,2)], "count_norep")

df_count_norep99 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(c(vars_mis9899, vars_noans99)), funs(count_norep99))
colnames(df_count_norep99)[-c(1,2)] <- paste0(colnames(df_count_norep99)[-c(1,2)], "count_norep")

df_resp <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize(n = n())

df_countna <- left_join(df_count_undec8, df_count_undec98, by = c("country", "studyno")) %>%
  left_join(df_count_norep9, by = c("country", "studyno")) %>%
  left_join(df_count_norep99, by = c("country", "studyno")) %>%
  left_join(df_resp, by = c("country", "studyno"))

cntry_rm <- c("AT-Austria", "CN-China", "HR-Croatia", "CZ-Czech Republic", "IN-India", "LT-Lithuania", 
              "PH-Phillippines", "RU-Russia", "SK-Slovak Republic", "CH-Switzerland", "US-United States")

df_countna_countryrm <- df_countna %>%
  filter(!(country %in% cntry_rm))

df_countna_countryrm[-c(1,2,ncol(df_countna_countryrm))] <- map_dfc(df_countna_countryrm[-c(1,2,ncol(df_countna_countryrm))], ~./df_countna_countryrm$n)

save_path_dta = file.path(data_path, "issp2017_countna.dta")
save_path_sav = file.path(data_path, "issp2017_countna.sav")
write_dta(df_countna_countryrm, save_path_dta, version = 14)
write_sav(df_countna_countryrm, save_path_sav)