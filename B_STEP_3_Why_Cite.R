## STEP 3: Why Authors Cited the UIUC Article
##Purpose: Analyze why UIUC articles wSere cited
##Input file(s): survey_b_results.csv
##Output file(s): b_write_ins_why_cited_UIUC_article.csv, b_summary_authors_cited_UIUC_article.csv

##library(tidyverse)
##library(stringr)

b <- read.csv("survey_b_results.csv", na.strings=c(""," ","NA"))

## trim just to the Q4 columns
citation <- select(b, 1,3, 10,11)

## write ins first
citation_wi <- citation[complete.cases(citation), ]
citation_wi <- select(citation_wi, -"Q4_B_why_cited", -"ResponseId_B")

write.csv(citation_wi, "b_write_ins_why_cited_UIUC_article.csv", row.names = FALSE)

##get just responses
citation_responses <- select(citation, -"Q4_B_other_specify", -"ResponseId_B")
citation_responses <- citation_responses[complete.cases(citation_responses), ]

##easier colmumn heading
names(citation_responses)[names(citation_responses)=="Q4_B_why_cited"]<-"explain_citation"

## remove special characters and condense from responses
explain <- str_replace_all(citation_responses$explain_citation, c(" " = "", "�۪" = "", "\\(" = "","\\)" = ""))

##replace with simplified answers
explain <- gsub("Other,pleasespecify:", "other", explain, fixed = TRUE)
explain <- gsub("Thepapercitedprovidedgeneralbackgroundforourwork", "background", explain, fixed = T)
explain <- gsub("Ourworkisanextensionofthecitedpaper", "extension", explain, fixed = T)
explain <- gsub("Ourworkaddressesasimilarprocessbutinadifferentcontext", "context", explain, fixed = T)
explain <- gsub("Ourworkusedalternativeorimprovedmethodology", "alternative", explain, fixed = T)
explain <- gsub("Ourpapervalidatedtheresultsofthecitedpaper", "validation", explain, fixed = T)

citation_r <- cbind(citation_responses, explain)
##double check were lined up correctly, then remove messy column
citation_r2 <- select(citation_r, -explain_citation)
citation_r3 <- separate_rows(citation_r2, explain, sep = ",")
citation_r3 <- citation_r3[,c(2,1)]
citation_r3 <- citation_r3[order(citation_r3$Discipline),]

#sum and condense disciplines
citation_r4 <- citation_r3 %>% 
  select(explain, Discipline) %>% 
    group_by(explain) %>%
      mutate(sum = length(explain)) %>%
        mutate(all_disciplines = toString(unique(Discipline)))

citation_r5 <- unique(select(citation_r4, -Discipline))

write.csv(citation_r5, "b_summary_authors_cited_UIUC_article.csv", row.names = FALSE)



