################################################################################
# UI of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:34:50
################################################################################


# Commute Explorer UI -----------------------------------------------------

commute_explorer_ui  <- fluidPage(
  
  navbarPage(paste("Engagement Strategy Efficacy Dashboard"),
             
             #tags$main(tags$h2("Get Window Dimensions Example"),
             #tags$p("Resize the browser."),
             #verbatimTextOutput("winSize")),
             tags$script(src = "index.js"),
             
             navbarMenu("Menu",
                        tabPanel("Heatmap",
                                 fluidRow(
                                   selectInput(inputId = "study_heatmap",
                                               label = "Select Study(s)",
                                               choices = unique(ascent_heatmap$STUDY_NUM),
                                               selected = unique(ascent_heatmap$STUDY_NUM)[1],
                                               multiple = T
                                   ),
                                 ),
                                 fluidRow(
                                   DTOutput('heatmap_table'),
                                   plotlyOutput('heatmap_lc')
                                 )
                                 
                        ),
                        tabPanel("Overall Efficacy Signals",
                                 fluidRow(
                                   column(width=8,selectInput(inputId = "study_ascent",
                                                              label = "Select Study(s)",
                                                              choices = c(unique(glmm_decay_results$study)),
                                                              selected = "AbbVie Overall",
                                                              multiple=T
                                   )),
                                   column(width=4,selectInput(inputId="function_select", 
                                                              label="Select CRM Functions(s)", 
                                                              choices = c("CRA","MSL"),
                                                              selected = c("CRA","MSL"),
                                                              multiple=T
                                   )),
                                 ),
                                 fluidRow(
                                   column(width=6,plotlyOutput(outputId = "window_plot")),
                                   column(width=6,plotlyOutput(outputId = "decay_plot"))
                                 )
                        ),
                        tabPanel("Recruitment Strategies/Barriers",
                                 fluidRow(
                                   column(width=4,selectInput(inputId = "study_ascent2",
                                                              label = "Select Study(s)",
                                                              choices = c(unique(glmm_recruitment$Study)),
                                                              selected = "AbbVie Overall",
                                                              multiple=T
                                   )),
                                   column(width=4,selectInput(inputId="topic_type_select", 
                                                              label="Select Topic Type(s)", 
                                                              choices = unique(glmm_recruitment$regressor_type),
                                                              selected = unique(glmm_recruitment$regressor_type),
                                                              multiple=T
                                   )),
                                   #column(width=4,selectInput(inputId="topic_select", 
                                   #                            label="Select Topic(s)", 
                                   #                            choices = unique(glmm_recruitment$Regressor),
                                   #                            selected = unique(glmm_recruitment$Regressor),
                                   #                            multiple=T
                                   # ))
                                 ),
                                 fluidRow(
                                   plotlyOutput(outputId = "rs_plot")                         
                                 )
                        )
             )
  ),
  
  
)



# App template ------------------------------------------------------------

htmlTemplate(
  filename = "www/index.html",
  commute_explorer = commute_explorer_ui
)
