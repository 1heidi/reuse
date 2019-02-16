## STEP 1: Data Exists
##Purpose: Determine Yes/No data by discipline
##Package(s): tidyverse, sjPlot
##Input file(s): All_Results_All_Answers_R_2019-02-15.csv
##Output file(s):

##library(tidyverse)
##library(sjPlot)

all <- read.csv("All_Results_All_Answers_R_2019-02-15.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

all[is.na(all)] <- 0  ## NA's are messing me up

all_data <- all %>%
  group_by(Discipline) %>%
    mutate(count_discip_response = length(Discipline)) %>%
      mutate(count_data_yes = sum(WasThereUnderlyingDataInYourPaper=="Yes")) %>%
        mutate(percent_data_yes = count_data_yes/count_discip_response) %>%
          mutate(count_used = sum(HasAnyoneUsedYourData=="Yes")) %>%
            mutate(percent_used = count_used/count_data_yes)

summary <- unique(select(all_data, Discipline, count_discip_response, count_data_yes, percent_data_yes, count_used, percent_used))

write.csv(summary, "Table1.csv", row.names = FALSE)
