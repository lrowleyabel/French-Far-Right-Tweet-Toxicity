####################################
#                                  #
#  RECODING INDEPENDENT VARIABLES  #
#                                  #
####################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 28/03/23

# DESCRIPTION: This file recodes the relevant independent variables to be used in the analysis of Tweet toxicity.

library(dplyr)
library(lubridate)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Load in data with Perspective API scores
load("../Data/sample_analysis_dataset.Rda")

# Recode variables gender and incumbency
sample_tweet_df<- sample_tweet_df%>%
  mutate(gender = case_when(gender == 0 ~ "Male",
                            gender == 1 ~ "Female"),
         incumbent = case_when(office1_incumbent == "0" ~ "Not incumbent",
                               office1_incumbent == "1" ~ "Incumbent",
                               office1_incumbent == "2" ~ NA_character_))


# Calculate age from birthdate
sample_tweet_df<- sample_tweet_df%>%
  mutate(age = interval(ymd(birthdate), dmy("12-06-2022"))%/%years(1)%>%
           floor())%>%
  select(-birthdate)

# Calculate highest office level
sample_tweet_df<- sample_tweet_df%>%
  mutate(across(contains("prior_high_pub_off_level"), ~as.numeric(.x)))

sample_tweet_df<- sample_tweet_df%>%
  rowwise()%>%
  mutate(highest_office = max(c_across(contains("prior_high_pub_off_level")), na.rm = T))

sample_tweet_df<- sample_tweet_df%>%
  mutate(highest_office_level = case_when(is.na(highest_office) ~ "None",
                                          is.infinite(highest_office) ~ "None",
                                          highest_office == 0 ~ "Local",
                                          highest_office == 1 ~ "Regional",
                                          highest_office >= 2 ~ "National or above"))%>%
  mutate(highest_office_level = factor(highest_office_level, levels = c("None", "Local", "Regional", "National or above")))


# Save data
save(sample_tweet_df, file = "../Data/sample_analysis_dataset.Rda" )
