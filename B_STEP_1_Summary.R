## STEP 1: Summary Across Disciplines
##Purpose: Determine counts of data from initial UIUC article used or not used, and access mechanisms (results within commnented code - no output)
##Package(s): tidyverse
##Input file(s): survey_b_results.csv
##Output file(s): b_overall_summary.csv

##library(tidyverse)

b <- read.csv("survey_b_results.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

b[is.na(b)] <- 0  ## NA's are messing me up

b_data <- b %>%
  group_by(Discipline) %>%
    mutate(count_discip_response = length(Discipline)) %>%
       mutate(percent_discip_response = 100*(count_discip_response/240)) %>%
        mutate(count_data_used = sum(Q1_B_UIUC_data_used=="Yes")) %>%
          mutate(percent_data_used = 100*count_data_used/240) %>%
            mutate(count_not_used = sum(Q1_B_UIUC_data_used=="No")) %>%
               mutate(percent_not_used = 100*count_not_used/240)

summary <- unique(select(b_data, Discipline, count_discip_response, percent_discip_response, count_data_used, percent_data_used, count_not_used, percent_not_used))

write.csv(summary, "b_summary_overall.csv", row.names = FALSE)

## Access mechanims
used_data <- filter(b,Q1_B_UIUC_data_used=="Yes")
used_data <- used_data[, 1:7]

used_access <- select(used_data, 3,5:7)
used_access <- filter(used_access, used_access$Q2_B_how_provided !=0)

access <- str_replace_all(used_access$Q2_B_how_provided, c(" " = "", "\\(" = "","\\)" = ""))
access <- gsub("Journalsupplement", "sup", access, fixed = TRUE)
access <- gsub("Copieddatafromtables,figures,etc.inthepaper", "paper", access, fixed = T)
access <- gsub("Viaemail", "email", access, fixed = T)
access <- gsub("Onlinedatabasesordatarepository,pleasespecify:", "db", access, fixed = T)
access <- gsub("Other,pleasespecify", "other", access, fixed = T)

access_r2 <- cbind(used_access, access)
access_r2 <- select(access_r2, 1, 5)
access_r3 <- separate_rows(access_r2, access, sep = ",")
access_r3 <- access_r3[,c(2,1)]
access_r3 <- access_r3[order(access_r3$Discipline),]

access_r4 <- access_r3  %>% 
  select(access, Discipline) %>% 
  group_by(access) %>%
  mutate(sum = length(access)) %>%
  mutate(all_disciplines = toString(unique(Discipline)))

access_r5 <- unique(select(access_r4, -Discipline))

##results
## note: two nonsensical write-in... "It was data I acquired. I wrote both papers." ... confused respondent... not author... 
## and "paper findings that tillage effects soil carbon and that it can vary signficantly" - answers why not how
# access   sum all_disciplines                   
# <chr>  <int> <chr>                             
# 1 db         4 BIOC, EART, ENGI, MEDI            
# 2 sup        9 BIOC, CHEM, ENGI, ENVI, PHYS      
# 3 paper     14 BIOC, ENGI, ENVI, MATE, MEDI, PHYS
# 4 other      5 CHEM, EART, ENGI, ENVI, MATE      
# 5 email      1 EART                              
