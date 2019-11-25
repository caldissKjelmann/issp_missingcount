## ISSP MISSING

#Sanne skal fremlægge 5/12 om mønstre i missing: hvilke spørgsmål, bliver ikke besvaret samt lande- og årssammenligning.

#Forsøg med et enkelt modul og lav datastruktur, der tæller missing for hver variabel for hvert land. 
#Script skal laves sådan, at det nemt kan udvides med flere datasæt.

library(haven)
library(tidyverse)

data_path <- "C:/Users/kjelm/OneDrive - Aalborg Universitet/CALDISS_projects/issp_missing_nov19/data/"
out_path <- "C:/Users/kjelm/OneDrive - Aalborg Universitet/CALDISS_projects/issp_missing_nov19/output/"

setwd(data_path)
issp2017 <- read_dta("ZA6980_v2-0-0.dta")
colnames(issp2017) <- str_replace(colnames(issp2017), "^v(?=\\d{1}$)", "v0")

#vector for all variable names of survey questions
surveyvars <- na.omit(str_extract(colnames(issp2017), "^v\\d{1,2}"))

#vector for variables using 98 and 99 for missing values
vars_mis9899 <- c("v36", "v37", "v44")

#vector for variables using only 99 for missing values
vars_noans99 <- c("v48", "v49", "v50", "v51", "v52", "v65", "v66")

#vector for variables using only 9 for missing values
vars_noans9 <- c("v60")

#vector for variables using 8 and 9 for missing values
vars_mis89 <- surveyvars[!(surveyvars %in% c(vars_mis9899, vars_noans99, vars_noans9))]

#subsetting data to include only studyno, country and survey questions
issp_subset <- issp2017 %>%
  select("studyno", "country", surveyvars) %>%
  mutate(country = as_factor(country))

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

#df for missing using 8
df_count_undec8 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(vars_mis89), funs(count_undec8))
colnames(df_count_undec8)[-c(1,2)] <- paste0(colnames(df_count_undec8)[-c(1,2)], "_count_undec")

#df for missing using 98
df_count_undec98 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(vars_mis9899), funs(count_undec98))
colnames(df_count_undec98)[-c(1,2)] <- paste0(colnames(df_count_undec98)[-c(1,2)], "_count_undec")

#df for missing using 9
df_count_norep9 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(c(vars_mis89, vars_noans9)), funs(count_norep9))
colnames(df_count_norep9)[-c(1,2)] <- paste0(colnames(df_count_norep9)[-c(1,2)], "_count_norep")

#df for missing using 99
df_count_norep99 <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize_at(vars(c(vars_mis9899, vars_noans99)), funs(count_norep99))
colnames(df_count_norep99)[-c(1,2)] <- paste0(colnames(df_count_norep99)[-c(1,2)], "_count_norep")

#df for n - number of respondents (in total per country)
df_resp <- issp_subset %>%
  group_by(country, studyno) %>%
  summarize(n = n())

#joining data frames
df_countna <- left_join(df_count_undec8, df_count_undec98, by = c("country", "studyno")) %>%
  left_join(df_count_norep9, by = c("country", "studyno")) %>%
  left_join(df_count_norep99, by = c("country", "studyno")) %>%
  left_join(df_resp, by = c("country", "studyno"))

#list of countries to filter out
cntry_rm <- c("AT-Austria", "CN-China", "HR-Croatia", "CZ-Czech Republic", "IN-India", "LT-Lithuania", 
              "PH-Phillippines", "RU-Russia", "SK-Slovak Republic", "CH-Switzerland", "US-United States")

#filtering out countries
df_countna_countryrm <- df_countna %>%
  filter(!(country %in% cntry_rm))

#df for proportions of missing (number of missing / n)
df_propna <- df_countna_countryrm
df_propna[-c(1,2,ncol(df_propna))] <- map_dfc(df_propna[-c(1,2,ncol(df_propna))], ~./df_propna$n)
colnames(df_propna)[-c(1,2)] <- str_replace(colnames(df_propna)[-c(1,2)], "count", "prop")

#joining
df_isspna_countprop <- left_join(df_countna_countryrm, df_propna, by = c("country", "studyno", "n")) %>%
  select(studyno, country, colnames(.)[order(colnames(.))])

#vectors of count variables
undecvars <- na.omit(str_extract(colnames(df_isspna_countprop), ".*_count_undec"))
norepvars <- na.omit(str_extract(colnames(df_isspna_countprop), ".*_count_norep"))

#adding totals
df_isspna_countprop$undec_total <- rowSums(df_isspna_countprop[, undecvars])
df_isspna_countprop$norep_total <- rowSums(df_isspna_countprop[, norepvars])

#outputting data files
save_path_dta = file.path(out_path, "issp2017_countna.dta")
save_path_sav = file.path(out_path, "issp2017_countna.sav")
write_dta(df_countna_countryrm, save_path_dta, version = 14)
write_sav(df_countna_countryrm, save_path_sav)