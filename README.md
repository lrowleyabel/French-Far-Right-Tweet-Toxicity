# French Far-Right Tweet Toxicity

This is code for a research project looking at the 'toxicity' of language used by far-right candidates in the 2022 French legislative election. The research is being carried out with the University of Nottingham and data collection was conducted with funding from [the Digital Society Project](http://digitalsocietyproject.org/).

The toxicity of the language used is modelled using the [Perspective API ML model](https://perspectiveapi.com/). Examples of how the Tweets are classified by the model can be seen in this [Shiny app](https://datavisualise.shinyapps.io/FrenchElectionTweetToxicityExamples/).

The code draws on datasets created in [the repository for the overall project](https://github.com/lrowleyabel/French-Election-Tweet-Analysis) that is looking at Twitter behaviour of candidates in the election more broadly.

**Workflow:**
- **Data preparation:**
   - Use the Perspective API model to calculate different dimensions of toxicity in the Tweet text: _calculating_perspective_api_scoring.R_
   - Recode the relevant independent variables to be used in the analysis: _recoding_independent_variables.R_
- **Analysis:**
  - Conduct simple bivariate analyses plotting the distribution of toxicity across the independent variables: _bivariate_analysis.R_
  - Run multilevel regressions to model the Tweet toxicity: _modelling_tweet_toxicity.R_
