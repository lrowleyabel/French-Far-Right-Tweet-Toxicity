library(dplyr)
library(ggplot2)
library(ggthemes)
library(lme4)
library(marginaleffects)
library(broom.mixed)
library(parameters)
library(broom)
library(gtsummary)
library(gtable)


rm(list = ls())

# Load sample of tweets with calculated scores from Perspective API
load("../Data/sample_analysis_dataset.Rda")

df<- sample_tweet_df


#### STEP 1: RUN MODELS AND CALCULATE MARGINAL EFFECTS ####


# Transform Toxicity variable
df<- df%>%
  mutate(logTox = log(TOXICITY))

# Run Model 1 (age, gender, party)
m1<- lmer(logTox ~ 1 + age + gender + party_list +  (1 | bioid), data = df)
summary(m1)

# Calculate marginal effects for party in Model 1
predictions(m1, newdata = datagrid(model = m1, party_list = c("Ensemble", "Les Républicains",  "Reconquête", "Rassemblement National")))%>%
  as.data.frame()%>%
  mutate(toxicity = exp(estimate))

# Run Model 2 (age, gender, highest offfice level)
m2<- lmer(logTox ~ 1 + age + gender + highest_office_level +  (1 | bioid), data = df)
summary(m2)

# Calculate marginal effects for highest office level in Model 2
predictions(m2, newdata = datagrid(model = m2, highest_office_level = c("None", "Local", "Regional", "National or above")))%>%
  as.data.frame()%>%
  mutate(toxicity = exp(estimate))

# Run Model 3 (age, gender, party, highest office level)
m3<- lmer(logTox ~ 1 + age + gender + party_list + highest_office_level +  (1 | bioid), data = df)
summary(m3)

# Plot marginal effects for both party and highest office level in Model 3
p1<- predictions(m3, newdata = datagrid(model = m3, highest_office_level = c("None", "Local", "Regional", "National or above"), party_list = c("Ensemble", "Les Républicains",  "Reconquête", "Rassemblement National")))%>%
  as.data.frame()%>%
  mutate(toxicity = exp(estimate))%>%
  ggplot()+
    annotate("segment", y = seq(0.02,0.1,0.01), yend = seq(0.02,0.1,0.01), x = 1, xend = 4, color = "white")+
    annotate("segment", y = 0.02, yend = 0.1, x = seq(1, 4, 0.25),xend = seq(1, 4, 0.25), color = "white")+
    geom_point(aes(x = as.numeric(highest_office_level), y = toxicity, shape = party_list, color = party_list))+
    geom_line(aes(x = as.numeric(highest_office_level), y = toxicity, group = party_list, color = party_list))+
    scale_color_brewer(palette = 6, type = "qual", name = "")+
    scale_shape(name = "")+
    scale_x_continuous(breaks = 1:4, labels = c("None", "Local", "Regional", "National or\nabove"))+
    theme_economist()+
  theme(axis.title.y = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(0, 0, 10, 0)),
        plot.title = element_text(margin = margin(10,0,10,0)),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.text.x = element_text(vjust = 1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9),
        plot.margin = margin(10,20,10,20))+
  labs(title = "Figure 4: Modelled Average Toxicity Levels by Party and\nHighest Office Level (Model 3)", subtitle = "N = 25,000", x = "Highest office level", caption = "Tweet data from Twitter API. Toxicity data from Perspective API.")


ggsave("Plots/Model 1 Estimates.png", plot = p1, units = "in", width = 8, height = 5, dpi = 1000)

# Filter for just RN and REQ candidates
df<- df%>%
  mutate(party_list = case_when(party_list == "Rassemblement National" ~ "Rassemblement National",
                           party_list == "Reconquête" ~ "Reconquête",
                           T ~ NA_character_))%>%
  mutate(party_list = factor(party_list, levels = c("Reconquête", "Rassemblement National")))

# Run Model 4 (age, gender, party, highest office level and party*highest office level interaction_)
m4<- lmer(logTox ~ 1 + age + gender + party_list + highest_office_level +  (party_list * highest_office_level) + (1 | bioid), data = filter(df, party_list %in% c("Reconquête", "Rassemblement National")))
summary(m4)

# Plot marginal effects for both party and highest office level in Model 4
shps<- c(3,15) # Set shapes used to represent parties in the plot, so that they're consister with previous plots
names(shps)<- c("Reconquête", "Rassemblement National")

p2<- predictions(m4, newdata = datagrid(model = m4, highest_office_level = c("None", "Local", "Regional", "National or above"), party_list = c("Reconquête", "Rassemblement National")))%>%
  as.data.frame()%>%
  mutate(toxicity = exp(estimate))%>%
  ggplot()+
  annotate("segment", y = seq(0.02,0.1,0.01), yend = seq(0.02,0.1,0.01), x = 1, xend = 4, color = "white")+
  annotate("segment", y = 0.02, yend = 0.1, x = seq(1, 4, 0.25),xend = seq(1, 4, 0.25), color = "white")+
  geom_point(aes(x = as.numeric(highest_office_level), y = toxicity, shape = party_list, color = party_list))+
  geom_line(aes(x = as.numeric(highest_office_level), y = toxicity, group = party_list, color = party_list))+
  scale_color_manual(values = c("#984EA3", "#4DAF4A"), name = "")+
  scale_shape_manual(values = shps, name = "")+
  scale_x_continuous(breaks = 1:4, labels = c("None", "Local", "Regional", "National or\nabove"))+
  theme_economist()+
  theme(axis.title.y = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(0, 0, 10, 0)),
        plot.title = element_text(margin = margin(10,0,10,0)),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.text.x = element_text(vjust = 1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9),
        plot.margin = margin(10,20,10,20))+
  labs(title = "Figure 5: Modelled Average Toxicity Levels by Party and\nHighest Office Level (Model 4)", subtitle = "N = 14,862", x = "Highest office level", caption = "Tweet data from Twitter API. Toxicity data from Perspective API.")

ggsave("Plots/Model 4 Estimates.png", plot = p2, units = "in", width = 8, height = 5, dpi = 1000)


#### STEP 2: EXPORT MODEL RESULTS AS HTML TABLE ####

# Specify function to get required parameters from the models (ie: the coefficients, confidence intervals and p-values for the fixed effects)
lmer_tidy<- function(m, exponentiate = F){
  tidied_model<- broom.mixed::tidy(m, effects = "fixed")  
  tidied_model$conf.int<- paste(parameters(m)$CI_low[1:nrow(tidied_model)], "-", parameters(m)$CI_high[1:nrow(tidied_model)])
  tidied_model$p.value<- parameters(m)$p[1:nrow(tidied_model)]
  if(exponentiate==T){
    tidied_model$estimate<- exp(tidied_model$estimate)
  }
  return(tidied_model)
}

# Combine the four  models into a list
models_list<- list(m1, m2, m3, m4)

# Specify function to produce a gt table for a given model 
presented_model<- function(m){tbl_regression(m,
                                             tidy_fun = function(x, ...) lmer_tidy(x),
                                             estimate_fun =  function(x, ...) round(x,3))}
    
# Create a list containing a gt table for each model
model_tables<- lapply(models_list, presented_model)

# Merge the four gt tables into one combined table and save
tbl_merge(model_tables, tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4"))
  as_gt()%>%
  tab_footnote(footnote = html("Note: Tweets at Level 1, Candidates at Level 2"))%>%
  tab_caption(md("**Table 1: Multilevel Regressions of Tweet Toxicity**"))
  gtsave(filename = "new_toxicity_model_results.html")

# Get the log likelihood and the number of observations for each model so we can report those too
logLik(m1)
logLik(m2)
logLik(m3)
logLik(m4)

nobs(m1)
nobs(m2)
nobs(m3)
nobs(m4)
