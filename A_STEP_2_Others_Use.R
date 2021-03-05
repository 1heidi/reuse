## STEP 2: Data Used or Not Used by Others
##Purpose: Analyze responses for data has been used, how important, access (results within commented code - no output), and then not used and reasons
##Package(s): tidyverse, stringr
##Input file(s): survey_a_results.csv
##Output file(s): a_summary_importance.csv, a_write_ins_why_data_not_used.csv, a_summary_why_data_not_used.csv

##library(tidyverse)
##library(stringr)

a <- read.csv("survey_a_results.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

## filter to only papers with data
with_data <- filter(a,Q1_A_was_data=="Yes")
with_data <- data.frame(lapply(with_data, trimws), stringsAsFactors = FALSE)

## DATA USED

## filter to only papers with data used
used_data <- filter(with_data,Q2_A_anyone_used=="Yes")
used_data <- used_data[, 1:9]

#copy df
used_access <- used_data ##used below
used_data2 <- used_data

# convert answers to numerics for averaging
used_data2$Q3_A_importance_to_them[used_data2$Q3_A_importance_to_them == "Extremely important"] <- 5
used_data2$Q3_A_importance_to_them[used_data2$Q3_A_importance_to_them == "Very important"] <- 4
used_data2$Q3_A_importance_to_them[used_data2$Q3_A_importance_to_them == "Somewhat important"] <- 3
used_data2$Q3_A_importance_to_them[used_data2$Q3_A_importance_to_them == "Not very important"] <- 2
used_data2$Q3_A_importance_to_them[used_data2$Q3_A_importance_to_them == "Not at all important"] <- 1

names(used_data2)[names(used_data2)=="Q3_A_importance_to_them"]<-"Importance_Numeric"

## just new column + ResponseID to make sure data not scrambled
used_data2 <- select(used_data2, ResponseId_A, Importance_Numeric)

## make sure rows align
used_data <- arrange(used_data, ResponseId_A)
used_data2 <- arrange(used_data2, ResponseId_A)
used_data3 <- merge(used_data, used_data2)

#reorder columns and rename data frame 
used_data <- used_data3[c(2,5,10)]

## remove responses that did not answer Importance Question (2)
used_importance <- used_data[complete.cases(used_data[1:3]),]

used_importance <-used_importance %>%
    group_by(Discipline) %>%
      mutate(N = length(Discipline)) %>%
        mutate(ave_importance = mean(as.numeric(Importance_Numeric))) %>%
          mutate(Extremely = sum(Q3_A_importance_to_them=="Extremely important")) %>%
            mutate(Very = sum(Q3_A_importance_to_them=="Very important")) %>%
              mutate(Somewhat = sum(Q3_A_importance_to_them=="Somewhat important")) %>%
                mutate(Not_very = sum(Q3_A_importance_to_them=="Not very important")) %>%
                  mutate(Not_at_all = sum(Q3_A_importance_to_them=="Not at all important"))

summary_importance <- unique(select(used_importance, Discipline, N, Extremely, Very, Somewhat, Not_very, Not_at_all, ave_importance))

write.csv(summary_importance, "a_summary_importance.csv", row.names = FALSE)

# access mechanisms for others' who used data

used_access <- select(used_access, 1,2,6,7)
used_access <- used_access[!is.na(used_access$Q4_A_how_provided), ]

access <- str_replace_all(used_access$Q4_A_how_provided, c(" " = "", "\\(" = "","\\)" = ""))
access <- gsub("Journalsupplement", "sup", access, fixed = TRUE)
access <- gsub("Author's/project'swebsite", "website", access, fixed = T)
access <- gsub("Viaemail", "email", access, fixed = T)
access <- gsub("Onlinedatabaseordatarepository,pleasespecify:", "db", access, fixed = T)
access <- gsub("Otherpleasespecify", "other", access, fixed = T)
access <- gsub("ViaBox,GoogleDrive,Drop-Box,FTP", "box", access, fixed = T)

access_r2 <- cbind(used_access, access)
access_r2 <- select(access_r2, 2, 5)
access_r3 <- separate_rows(access_r2, access, sep = ",")
access_r3 <- access_r3[,c(2,1)]
access_r3 <- access_r3[order(access_r3$Discipline),]

access_r4 <- access_r3  %>% 
  select(access, Discipline) %>% 
      group_by(access) %>%
        mutate(sum = length(access)) %>%
          mutate(all_disciplines = toString(unique(Discipline)))

access_r5 <- unique(select(access_r4, -Discipline))

##results - NOTE the one writein for "other" also counts at website...  "lab's web site"
##access    sum all_disciplines       
# <chr>   <int> <chr>                 
# 1 sup         5 CHEM, ENVI, MEDI, PHYS
# 2 website     3+1 CHEM, COMP            
# 3 email       4 CHEM, ENVI, MEDI      
# 4 db          2 COMP, MEDI            
# 5 other       1-1 ENGI                  
# 6 box         1 MEDI                

## DATA NOT USED

# filter to only papers with data NOT used 
data_not_used <- filter(with_data,Q2_A_anyone_used=="No")
data_not_used <- data_not_used[c(2, 4, 9, 10)]

##get just list of write-in comments 
data_not_used_wi <- data_not_used[complete.cases(data_not_used), ]
data_not_used_wi <- select(data_not_used_wi, -"Q5_A_reason_not_used")

write.csv(data_not_used_wi, "a_write_ins_why_data_not_used.csv", row.names = FALSE)

##get just responses
data_not_used_r <- select(data_not_used, -"Q5_A_other_specify")

## remove special characters and condense from responses
fix_explain <- str_replace_all(data_not_used_r$Q5_A_reason_not_used, c(" " = "", "\\(" = "","\\)" = ""))

##replace with simplified answers
fix_explain <- gsub("Otherpleasespecify", "other", fix_explain, fixed = TRUE)
fix_explain <- gsub("ItmayhavebeentoocomplicatedtouseourdataduetolegalorIPrestrictionse.g.,suchasdatauseagreements,orclassifiedorrestrictedaccessdata", "ip", fix_explain, fixed = T)
fix_explain <- gsub("Wedidnotprovideunderlyingdata", "not_shared", fix_explain, fixed = T)
fix_explain <- gsub("Thedatamaynothaveaddressedtheirspecificresearchquestione.g.,theymayhaveneededtocollectthedatawiththeirownexperimentalcontrols,conditions,instrumentsand/ormaterials,etc.", "research_question", fix_explain, fixed = T)
fix_explain <- gsub("Theymaynotbeconfidentinthequalityofourdata", "confidence", fix_explain, fixed = T)
fix_explain <- gsub("Theymaythinkit'simportantfortheirstudents'educationtolearntocollecttheirowndata", "students", fix_explain, fixed = T)
fix_explain <- gsub("Itmayhavebeentoodifficulttounderstandorreformatourdata", "understand_reformat", fix_explain, fixed = T)

data_not_used_r2 <- cbind(data_not_used_r, fix_explain)
##double check were lined up correctly, then remove messy column
data_not_used_r2 <- select(data_not_used_r2, -Q5_A_reason_not_used, -Q2_A_anyone_used)
data_not_used_r3 <- separate_rows(data_not_used_r2, fix_explain, sep = ",")
data_not_used_r3 <- data_not_used_r3[,c(2,1)]
data_not_used_r3 <- data_not_used_r3[order(data_not_used_r3$Discipline),]

#sum and condense disciplines
data_not_used_r4 <- data_not_used_r3 %>% 
    select(fix_explain, Discipline) %>% 
        group_by(fix_explain) %>%
            mutate(sum = length(fix_explain)) %>%
                mutate(all_disciplines = toString(unique(Discipline)))

data_not_used_r5 <- unique(select(data_not_used_r4, -Discipline))
data_not_used_r5 <- data_not_used_r5[complete.cases(data_not_used_r5), ]

write.csv(data_not_used_r5, "a_summary_why_data_not_used.csv", row.names = FALSE)



