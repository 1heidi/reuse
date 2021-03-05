## STEP 2: Why Citing Authors Did Not Use Data from the UIUC article
##Purpose: Analyze why data has not been used
##Package(s): tidyverse, stringr
##Input file(s): survey_b_results.csv
##Output file(s): b_write_ins_why_data_not_used.csv, b_summary_why_data_not_used.csv

##library(tidyverse)
##library(stringr)

b <- read.csv("survey_b_results.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

## filter to only papers with data used
data_not_used <- filter(b,Q1_B_UIUC_data_used=="No")
data_not_used <- data_not_used[c(1:4,8,9)]

## write ins first
data_not_used_wi <- data_not_used[complete.cases(data_not_used), ]
data_not_used_wi <- select(data_not_used_wi, -"Q3_B_why_opt_not_use", -"ResponseId_A", -"Q1_B_UIUC_data_used")

write.csv(data_not_used_wi, "b_write_ins_why_data_not_used.csv", row.names = FALSE)

##get just responses
data_not_used_r <- select(data_not_used, -"Q3_B_other_specify",-"ResponseId_A", -"Q1_B_UIUC_data_used")
data_not_used_r <- data_not_used_r[complete.cases(data_not_used_r), ]

## remove special characters and condense from responses
fix_explain <- str_replace_all(data_not_used_r$Q3_B_why_opt_not_use, c(" " = "", "\\(" = "","\\)" = ""))

##replace with simplified answers
fix_explain <- gsub("Other,pleasespecify:", "other", fix_explain, fixed = TRUE)
fix_explain <- gsub("ItwouldbehavetoocomplicatedtousethedataduetodatauseagreementsorothersIPrestrictions", "ip", fix_explain, fixed = T)
fix_explain <- gsub("Wedidnotprovideunderlyingdata", "not_shared", fix_explain, fixed = T)
fix_explain <- gsub("Datadidnotaddressourspecificresearchquestion", "research_question", fix_explain, fixed = T)
fix_explain <- gsub("Theymaynotbeconfidentinthequalityofourdata", "confidence", fix_explain, fixed = T)
fix_explain <- gsub("Itisimportantformystudents'educationtolearntocollecttheirowndata", "students", fix_explain, fixed = T)
fix_explain <- gsub("Therewasnodatainthepaper", "no_data", fix_explain, fixed = T)
fix_explain <- gsub("Itmayhavebeentoodifficulttounderstandorreformatourdata", "understand_reformat", fix_explain, fixed = T)

data_not_used_r2 <- cbind(data_not_used_r, fix_explain)
##double check were lined up correctly, then remove messy column
data_not_used_r2 <- select(data_not_used_r2, -ResponseId_B, -Q3_B_why_opt_not_use)
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

## NOTE: R_2A0Cit6jHPMUx7a indicated "other" in Q3 count is 81, but did not provide an answer, so only have 80 comments

write.csv(data_not_used_r5, "b_summary_why_data_not_used.csv", row.names = FALSE)



