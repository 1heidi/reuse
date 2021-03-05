## STEP 3: If Initial Article Used Others' Data
##Purpose: Analyze did data was reused in the initial UIUC article, importance or why data not used, and access (results within commented code - no output)
##Package(s): tidyverse, stringr
##Input file(s): survey_a_results.csv
##Output file(s): a_summary_importance_UIUC_data_used.csv, a_write_ins_UIUC_why_data_not_used.csv,a_summary_UIUC_why_data_not_used.csv

##library(tidyverse)
##library(stringr)

a <- read.csv("survey_a_results.csv", na.strings=c(""," ","NA"))

## trim just to the "if you used data" columns
others_data <- select(a, 1:3, 11:17)

## SET UP DATA FRAMES

## filter to only papers with data and then only that answered question about if used others data
others_data <- filter(others_data,Q1_A_was_data=="Yes")
others_data <- data.frame(lapply(others_data, trimws), stringsAsFactors = FALSE)
others_ans <- others_data[!is.na(others_data$Q6_A_UIUC_reuse),]

others_yes <- filter(others_ans,Q6_A_UIUC_reuse =="Yes")
others_no <- filter(others_ans,Q6_A_UIUC_reuse =="No")
others_access <- others_yes 

## FOR YES - USED OTHERS' DATA

## convert answers to numerics for averaging for importance of data uses
others_yes$Q7_A_importance_to_you[others_yes$Q7_A_importance_to_you == "Extremely important"] <- 5
others_yes$Q7_A_importance_to_you[others_yes$Q7_A_importance_to_you == "Very important"] <- 4
others_yes$Q7_A_importance_to_you[others_yes$Q7_A_importance_to_you == "Somewhat important"] <- 3
others_yes$Q7_A_importance_to_you[others_yes$Q7_A_importance_to_you == "Not very important"] <- 2
others_yes$Q7_A_importance_to_you[others_yes$Q7_A_importance_to_you =="Not at all important"] <- 1

names(others_yes)[names(others_yes)=="Q7_A_importance_to_you"]<-"Importance_Numeric"

others_yes <- others_yes  %>%
  group_by(Discipline) %>%
    mutate(N = length(Discipline)) %>%
       mutate(ave_importance = mean(as.numeric(Importance_Numeric))) %>%
          mutate(Extremely = sum(Importance_Numeric==5)) %>%
            mutate(Very = sum(Importance_Numeric==4)) %>%
              mutate(Somewhat = sum(Importance_Numeric==3)) %>%
                mutate(Not_very = sum(Importance_Numeric==2)) %>%
                  mutate(Not_at_all = sum(Importance_Numeric==1))

others_yes_summary <- unique(select(others_yes, 2,11,13:17, 12))
others_yes_summary <- others_yes_summary[,c(1,2,4:8,3)]
others_yes_summary <- others_yes_summary[order(others_yes_summary$Discipline),] ## throws Unknown or uninitialised column: error due to known bug but result is okay https://stackoverflow.com/questions/39041115/fixing-a-multiple-warning-unknown-column

write.csv(others_yes_summary, "a_summary_importance_UIUC_data_used.csv", row.names = FALSE)

# access mechanisms for others' who used data

others_access <- select(others_access, 1,2,6,7,8)

access <- str_replace_all(others_access$Q8_A_data_obtained_by_you, c(" " = "", "\\(" = "","\\)" = ""))
access <- gsub("Journalsupplement", "sup", access, fixed = TRUE)
access <- gsub("Author's/project'swebsite", "website", access, fixed = T)
access <- gsub("Viaemail", "email", access, fixed = T)
access <- gsub("Onlinedatabaseordatarepository,pleasespecify:", "db", access, fixed = T)
access <- gsub("Otherpleasespecify", "other", access, fixed = T)
access <- gsub("ViaBox,GoogleDrive,Drop-Box,FTP", "box", access, fixed = T)

access_r2 <- cbind(others_access, access)
access_r2 <- select(access_r2, 2, 6)
access_r3 <- separate_rows(access_r2, access, sep = ",")
access_r3 <- access_r3[,c(2,1)]
access_r3 <- access_r3[order(access_r3$Discipline),]

access_r4 <- access_r3  %>% 
  select(access, Discipline) %>% 
  group_by(access) %>%
  mutate(sum = length(access)) %>%
  mutate(all_disciplines = toString(unique(Discipline)))

access_r5 <- unique(select(access_r4, -Discipline))

## ACCESS results
# access    sum all_disciplines       
# <chr>   <int> <chr>                 
# 1 db          6 CHEM, COMP, MEDI, PHYS
# 2 sup         4 CHEM, EART, MEDI      
# 3 email       2 COMP, ENVI            
# 4 website     3 COMP, ENVI, PHYS      
# 5 other       2 ENGI, MATE            
# 6 box         1 ENVI    

## FOR NO - DID NOT USE OTHERS' DATA

others_no <- others_no[c(1:3,4, 9, 10)]
others_no  <- others_no[!is.na(others_no$Q9_A_reason_not_used_UIUC_author),]

##get just list of write-in comments 
others_no_wi <- others_no[complete.cases(others_no), ]
others_no_wi <- select(others_no_wi, -Q9_A_reason_not_used_UIUC_author)
others_no_wi <- select(others_no_wi, -ResponseId_A)

write.csv(others_no_wi, "a_write_ins_UIUC_why_data_not_used.csv", row.names = FALSE)

##get just responses
others_no <- select(others_no, -"Q9_A_other_specify", -"ResponseId_A", -"Q6_A_UIUC_reuse", -"Q1_A_was_data")

##easier colmumn heading
names(others_no)[names(others_no)=="Q9_A_reason_not_used_UIUC_author"]<-"explain_not_used"

## remove special characters and condense from responses
fix_explain <- str_replace_all(others_no$explain_not_used, c(" " = "", "�۪" = "", "\\(" = "","\\)" = ""))

##replace with simplified answers
fix_explain <- gsub("Otherpleasespecify", "other", fix_explain, fixed = TRUE)
fix_explain <- gsub("Others'datadidnotaddressourspecificresearchquestione.g.neededtocollectthedatawithownexperimentalcontrols,conditions,instrumentsand/ormaterials,etc.", "research_question", fix_explain, fixed = T)
fix_explain <- gsub("Wearenotconfidentaboutthequalityofothersdata", "confidence", fix_explain, fixed = T)
fix_explain <- gsub("Ourstudentscollectedthedata", "students", fix_explain, fixed = T)
fix_explain <- gsub("Itwouldhavebeentoodifficulttounderstandorreformatothersdata", "understand_reformat", fix_explain, fixed = T)

others_no_r <- cbind(others_no, fix_explain)
##double check were lined up correctly, then remove messy column
others_no_r2 <- select(others_no_r, -explain_not_used)
others_no_r3 <- separate_rows(others_no_r2, fix_explain, sep = ",")
others_no_r3 <- others_no_r3[,c(2,1)]
others_no_r3 <- others_no_r3[order(others_no_r3$Discipline),]

#sum and condense disciplines
others_no_r4 <- others_no_r3 %>% 
  select(fix_explain, Discipline) %>% 
    group_by(fix_explain) %>%
      mutate(sum = length(fix_explain)) %>%
        mutate(all_disciplines = toString(unique(Discipline)))

others_no_r5 <- unique(select(others_no_r4, -Discipline))

write.csv(others_no_r5, "a_summary_UIUC_why_data_not_used.csv", row.names = FALSE)



