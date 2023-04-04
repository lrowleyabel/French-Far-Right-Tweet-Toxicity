library(shiny)
library(dplyr)
library(bslib)
library(htmltools)
library(ggplot2)

js <- '
$(window).on("message", function(e) {
  var oe = e.originalEvent;
  if (oe.origin !== "https://twitframe.com")
    return;
  if (oe.data.height && oe.data.element.id.startsWith("tweet_")){
    $("#" + oe.data.element.id).css("height", parseInt(oe.data.height) + "px");
  }
});'

ui <- fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  fluidRow(
    tags$head(
      tags$script(HTML(js)),
      tags$style(HTML(
        "
        .content {
          margin: auto;
          padding: 20px;
          width: 80%;
        }
        
        body {
          padding:50px;
        }
        "))
    ),
    
    card(
      h1("Toxic Twitter Behaviour in the French 2022 Legislative Elections")
    ),
    
    layout_column_wrap(
      width = NULL, fill = FALSE,
      height = "100%",
      style = css(grid_template_columns = "1fr 2fr"),
      card(
        card_header("App Description"),
        card_body(
          div("This app displays a small sample of Tweets from right-wing/centrist candidates in the French 2022 legislative election and rates their level of toxic language across several dimensions. Using the Perspective API machine learning model, Tweets are scored on the dimensions of toxicity, severe toxicity, identity attack, profanity and threat", tags$a("(see here for descriptions).", href ="https://developers.perspectiveapi.com/s/about-the-api-attributes-and-languages?language=en_US"), "Only candidates from Rassemblement National, Reconquête, Les Républicains and Ensemble are included.", br(), br(), "You can explore how the model rated different Tweets by selecting a dimension and using the slider below to choose the range from which you want to see example Tweets. All dimensions are measured on a scale from 0 to 1."),
          br(),
          selectInput("dimension", "Select dimension:", choices = c("Toxicity", "Severe toxicity", "Identity attack", "Profanity", "Threat")),
          plotOutput("density_plot", height = 200, width = "95%"),
          sliderInput("toxicity_range", min = 0, max = 1, step = 0.01, value = c(0,1), label = ""),  
        )
      ),
      card(
        card_header("Example Tweets"),
        card_body(uiOutput("frame"))
      )
    )
  )
)


server <- function(input, output, session) {
  
    load("Data/sample_analysis_dataset.Rda")
    
    sample_tweet_df<- sample_tweet_df%>%
      rename(Toxicity = TOXICITY,
             `Severe toxicity` = SEVERE_TOXICITY,
             `Identity attack` = IDENTITY_ATTACK_EXPERIMENTAL,
             Profanity = PROFANITY_EXPERIMENTAL,
             Threat = THREAT_EXPERIMENTAL)
  
  
    output[["density_plot"]]<- renderPlot({
      
      sample_tweet_df$Dimension<- sample_tweet_df[[input$dimension]]
      
      d<- density(sample_tweet_df$Dimension)
      max_density<- max(d$y)
      
      
      ggplot(sample_tweet_df)+
        geom_density(aes(x = Dimension), alpha = 0.9, fill = "#78c2ad", color = NA)+
        #geom_vline(aes(xintercept = input$toxicity_range[1]))+
        annotate(geom = "rect", xmin = 0, xmax = input$toxicity_range[1], ymin = 0, ymax = max_density, alpha = 0.3)+
        #geom_vline(aes(xintercept = input$toxicity_range[2]))+
        annotate(geom = "rect", xmin = input$toxicity_range[2], xmax = 1, ymin = 0, ymax = max_density, alpha = 0.3)+
        xlim(0,1)+
        theme_void()
      
    }, height = 200)
  
  
    output[["frame"]] <- renderUI({
      
      sample_tweet_df$Dimension<- sample_tweet_df[[input$dimension]]
      
      tmp<- sample_tweet_df%>%
        ungroup()%>%
        filter(Dimension >= input$toxicity_range[1] & Dimension <= input$toxicity_range[2])%>%
        slice_sample(n = 10)
      
      lapply(1:nrow(tmp), function(i){
        
        tweet <- paste0("https://twitter.com/", tmp$user_screen_name[i], "Twitter/status/", tmp$text_id[i])
        url <- URLencode(tweet, reserved = TRUE)
        src <- paste0("https://twitframe.com/show?url=", url)
        
        tagList(
          tags$div(
            
            class = "content",
            
            tags$h3(
              paste0(paste(tmp$fname_orig[i], tmp$lname_orig[i]))
            ),
            
            tags$div(
              paste0(tmp$party_name_original[i])
            ),
            
            tags$div(
              paste0("Tweet ID: ", tmp$text_id[i])
            ),
            
            br(),
            
            tags$div(
              style = "width: 80%; display: grid; grid-template-columns: 30% 70%;",
              tags$span(
                style = "background-color: #eeeee4; width: 100px; height: 10px; display: block; border-radius: 9px; border: 2px solid #5a5a5a; margin: 10px 0px; float: left;",
                tags$span(style = paste0("background-color: #78c2ad; width:", tmp$Toxicity[i]*100, "px; height: 6px; display: block;border-radius: 9px; float: left;"))
              ),
              tags$div(
                style = "float: left;",
                paste0("Toxicity: ", round(tmp$Toxicity[i], 3))
              ),
              
              tags$span(
                  style = "background-color: #eeeee4; width: 100px; height: 10px; border-radius: 9px; border: 2px solid #5a5a5a; margin: 10px 0px; float: left;",
                  tags$span(style = paste0("background-color: #78c2ad; width:", tmp$`Severe toxicity`[i]*100, "px; height: 6px; display: block;border-radius: 9px; float: left;"))
              ),
              tags$div(
                  style = "float: left;",
                  paste0("Severe toxicity: ", round(tmp$`Severe toxicity`[i], 3))
              ),
              
              
              tags$span(
                style = "background-color: #eeeee4; width: 100px; height: 10px; border-radius: 9px; border: 2px solid #5a5a5a; margin: 10px 0px; float: left;",
                tags$span(style = paste0("background-color: #78c2ad; width:", tmp$`Identity attack`[i]*100, "px; height: 6px; display: block;border-radius: 9px; float: left;"))
              ),
              tags$div(
                style = "float: left;",
                paste0("Identity attack: ", round(tmp$`Identity attack`[i], 3))
              ),
              
              tags$span(
                style = "background-color: #eeeee4; width: 100px; height: 10px; border-radius: 9px; border: 2px solid #5a5a5a; margin: 10px 0px; float: left;",
                tags$span(style = paste0("background-color: #78c2ad; width:", tmp$Profanity[i]*100, "px; height: 6px; display: block;border-radius: 9px; float: left;"))
              ),
              tags$div(
                style = "float: left;",
                paste0("Profanity: ", round(tmp$Profanity[i], 3))
              ),
              
              tags$span(
                style = "background-color: #eeeee4; width: 100px; height: 10px; border-radius: 9px; border: 2px solid #5a5a5a; margin: 10px 0px; float: left;",
                tags$span(style = paste0("background-color: #78c2ad; width:", tmp$Threate[i]*100, "px; height: 6px; display: block;border-radius: 9px; float: left;"))
              ),
              tags$div(
                style = "float: left;",
                paste0("Threat: ", round(tmp$Threat[i], 3))
              )
            ),
            
            br(),
            
            tags$div(tags$iframe(
              id = paste0("tweet_", i),
              class = "tweet",
              border=0, frameborder=0, height = 300, width="100%",
              src = src
            ))
            
          ),
          
          tags$hr(),
          
          # tags$script(HTML(
          #   "$(document).ready(function(){
          #     $('iframe.tweet').on('load', function() {
          #       this.contentWindow.postMessage(
          #         { element: {id:this.id}, query: 'height' },
          #         'https://twitframe.com');
          #     });
          #    });"))
        )

      })
    })
    
}

shinyApp(ui, server)