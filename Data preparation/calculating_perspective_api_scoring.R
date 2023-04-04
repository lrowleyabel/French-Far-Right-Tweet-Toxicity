#########################################
#                                       #
#  CALCULATING PERSPECTIVE API SCORING  #
#                                       #
#########################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 22/03/23

# DESCRIPTION: This file uses the Perspective API to estimate the level of 'toxic' language use in candidate Tweets, creating separate
# scoring for toxicity, severe toxicity, identity attack, profanity and threat.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(MetBrewer)
library(utils)
library(lubridate)
library(readxl)
library(peRspective)
library(ggridges)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/French Election Project/Discourse Paper/Analysis")
rm(list = ls())

# Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets
data_dir<- choose.dir(caption = "Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets")


#### Step 1: Set up data ####


# Load in tweet dataset which contains one observation for each tweet
load(paste0(data_dir, "//Tweet Data//Rda Formatted Data//Tweet Data.Rda"))

# Remove duplicate tweets
tweet_df<- tweet_df[!duplicated(tweet_df$id_str),]

# Read in the lookup to join the Twitter accounts to the relevant BIOIDs
lookup<- read.csv("..//Lookups//BIOID_to_twitter_handle_lookup.csv")

# Join BIOIDs to tweet dataframe
tweet_df<- left_join(tweet_df, lookup, by = c("user_screen_name" = "Account_Handle"))

# Parse tweet date variable to date format
tweet_df<- tweet_df%>%
  mutate(date = parse_date_time(created_at, "a b d H:M:S z Y")%>%
           lubridate::date())

# Filter tweets to just keep those from 90 days before the campaign period onwards
tweet_df<- tweet_df%>%
  filter(date >= dmy("30-05-2022")-90)

# Filter tweets to just keep original tweets
tweet_df<- tweet_df%>%
  filter(is_retweet==F)

# Get directory containing candidate data
candidate_data_dir<- choose.dir(caption = "Select directory on University of Nottingham OneDrive (TplF - Working Dataset)")

# Read in candidate data
candidate_df<- read_excel(paste0(candidate_data_dir, "//Substantive Data Combined Candidates 05-01-23.xlsx"))

# Convert BIOID variable in candidate dataframe to integer
candidate_df<- candidate_df%>%
  mutate(BIOID = as.integer(BIOID))

# Join tweet dataframe and candidate dataframe so that we have one account per row (with repeated candidates where necessary)
tweet_df<- left_join(tweet_df, candidate_df, by = "BIOID")

# Standardise capitalisation of variable names
colnames(tweet_df)<- str_to_lower(colnames(tweet_df))

# Keep only relevant variables
tweet_df<- tweet_df%>%
  select(id_str, full_text, is_retweet, is_quote_status, date, user_screen_name, # twitter variables
         bioid, lname_orig, fname_orig, party_name_original, listname_original, independent, # candidate and party variables
         gender, birthdate, # demographic variables
         office1_incumbent, office1_prior_natl_legis_count, prior_high_pub_off_name_1:other_political_experience) # Previous experience variables

# Create a variable indicating the main parties
tweet_df<- tweet_df%>%
  mutate(party_list = case_when(listname_original == "Ensemble" ~ "Ensemble",
                                listname_original == "Rassemblement National" ~ "Rassemblement National",
                                listname_original == "La Nouvelle Union populaire écologique et sociale" ~ "NUPES",
                                listname_original == "Les Républicains" ~ "Les Républicains",
                                listname_original == "Reconquête" ~ "Reconquête",
                                independent == 1 ~ "Independent",
                                T ~ "Other party"))

# Save analysis dataset
save(tweet_df, file = "../Data/tweet_data_analysis_subset.Rda")
load("../Data/tweet_data_analysis_subset.Rda")


##### Step 2: Run Perspective API model on sample of Tweets ####


# Count number of tweets by main party
tweet_df%>%
  count(party_list)

# Filter tweet dataset to only relevant parties
tweet_df<- tweet_df%>%
  filter(party_list %in% c("Ensemble", "Les Républicains", "Rassemblement National", "Reconquête"))

n_distinct(tweet_df$bioid)

# Create a sample of 10% of the tweets to test
sample_tweet_df<- tweet_df%>%
  slice_sample(prop = 0.1)

# Calculate estimated time to run Perspective model on sample dataset
nrow(sample_tweet_df)/60/60

# Get scoring from Perspective API model for the sample dataset
results<- sample_tweet_df%>%
  prsp_stream(text = full_text, text_id = id_str, languages = "fr", score_model = c("TOXICITY", "SEVERE_TOXICITY", "IDENTITY_ATTACK_EXPERIMENTAL", "PROFANITY_EXPERIMENTAL", "THREAT_EXPERIMENTAL"), verbose = T)

# Join results from Perspective API to the sample dataset
sample_tweet_df<- left_join(results, sample_tweet_df, by = c("text_id" = "id_str"))

# Save the results
save(sample_tweet_df, "../Data/sample_analysis_dataset.Rda")

