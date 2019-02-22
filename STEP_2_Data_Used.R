## STEP 2: Data Used
##Purpose: Analyze when data has been used, how important was it and how data accessed
##Package(s): tidyverse
##Input file(s): All_Results_All_Answers_R_2019-02-15.csv
##Output file(s): summary_importance.csv

##library(tidyverse)

all <- read.csv("All_Results_All_Answers_R_2019-02-15.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

## note - keeping NAs

## filter to only papers with data
with_data <- filter(all,WasThereUnderlyingDataInYourPaper=="Yes")

## filter to only papers with data used
used_data <- filter(all,HasAnyoneUsedYourData=="Yes")
used_data <- used_data[, 1:9]

#copy df
used_data2 <- used_data

# convert answers to numerics for averaging
used_data2$HowImportantYouThinkItWasToTheirWork[used_data2$HowImportantYouThinkItWasToTheirWork == "Extremely important"] <- 5
used_data2$HowImportantYouThinkItWasToTheirWork[used_data2$HowImportantYouThinkItWasToTheirWork == "Very important"] <- 4
used_data2$HowImportantYouThinkItWasToTheirWork[used_data2$HowImportantYouThinkItWasToTheirWork == "Somewhat important"] <- 3
used_data2$HowImportantYouThinkItWasToTheirWork[used_data2$HowImportantYouThinkItWasToTheirWork == "Not very important"] <- 2
used_data2$HowImportantYouThinkItWasToTheirWork[used_data2$HowImportantYouThinkItWasToTheirWork == "Not at all important"] <- 1

names(used_data2)[names(used_data2)=="HowImportantYouThinkItWasToTheirWork"]<-"Importance_Numeric"

## just new column + ResponseID to make sure data not scrambled
used_data2 <- select(used_data2, ResponseId, Importance_Numeric)

## make sure rows align
used_data <- arrange(used_data, ResponseId)
used_data2 <- arrange(used_data2, ResponseId)
used_data3 <- merge(used_data, used_data2)

#reorder columns and rename data frame 
used_data <- used_data3[c(2, 1, 3, 4, 5, 6, 10, 7, 8, 9)]

## remove responses that did not answer HowImportantYouThinkItWasToTheirWork (e.g. NAs)
used_importance <- used_data[complete.cases(used_data[ , 6]),]

used_importance <-used_importance  %>%
  group_by(Discipline) %>%
    mutate(N = length(Discipline)) %>%
      mutate(ave_importance = mean(as.numeric(Importance_Numeric))) %>%
        mutate(Extremely = sum(HowImportantYouThinkItWasToTheirWork=="Extremely important")) %>%
          mutate(Very = sum(HowImportantYouThinkItWasToTheirWork=="Very important")) %>%
            mutate(Somewhat = sum(HowImportantYouThinkItWasToTheirWork=="Somewhat important")) %>%
              mutate(Not_very = sum(HowImportantYouThinkItWasToTheirWork=="Not very important")) %>%
                mutate(Not_at_all = sum(HowImportantYouThinkItWasToTheirWork=="Not at all important"))

summary_importance <- unique(select(used_importance, Discipline, N, ave_importance, Extremely, Very, Somewhat, Not_very, Not_at_all))

write.csv(summary_importance, "summary_importance.csv", row.names = FALSE)
  
  
  



