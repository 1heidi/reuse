## STEP 1: Summary Across Disciplines
##Purpose: Determine counts of data w/in articles and used by discipline
##Package(s): tidyverse
##Input file(s): survey_a_results.csv
##Output file(s): a_overall_summary.csv

##library(tidyverse)

a <- read.csv("survey_a_results.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

a[is.na(a)] <- 0  ## NA's are messing me up

## should work... all <- all[!is.na(DF$column),]

a_data <- a %>%
  group_by(Discipline) %>%
  mutate(count_discip_response = length(Discipline)) %>%
  mutate(count_data_yes = sum(Q1_A_was_data=="Yes")) %>%
  mutate(percent_data_yes = count_data_yes/count_discip_response) %>%
  mutate(count_used = sum(Q2_A_anyone_used=="Yes")) %>%
  mutate(percent_used = count_used/count_data_yes)

summary <- unique(select(a_data, Discipline, count_discip_response, count_data_yes, percent_data_yes, count_used, percent_used))

write.csv(summary, "a_summary_overall.csv", row.names = FALSE)
Catherine