rm(list=ls())

library(tidyr)
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)
library(DT)
library(plotly)
library(zoo)

cra_color = "#4e79a7"
msl_color = "#d27dcf"

barrier_red = "#e05759"
rs_blue = "#4d78a5"



#setwd("C:/Users/BRAKSAX/Documents/site-engagement/ASCENT app")


heatmap <-             read.csv("heatmap.csv") %>% 
  filter(!is.na(STUDY_NUM)) %>% 
  filter(year > 2000) %>% 
  mutate(Date = as.Date(paste0(year,"-",month,"-","01"),"%Y-%b-%d"))
glmm_window_results <- read.csv("glmm_window_results.csv")
glmm_decay_results <-  read.csv("glmm_decay_output.csv")
glmm_recruitment   <-  read.csv("recruitment_glmm_df.csv")
ascent_heatmap     <-  read.csv("ascent_heatmap_final.csv",check.names = F)

min_msl_df = heatmap %>% filter(event=="MSL") %>%
  filter(cnt>0) %>% 
  group_by(STUDY_NUM)%>%
  summarise(min_msl = min(Date)) %>% 
  ungroup()

min_cra_df = heatmap %>% filter(event=="CRA") %>%
  filter(cnt>0) %>% 
  group_by(STUDY_NUM)%>%
  summarise(min_cra = min(Date)) %>% 
  ungroup()

min_screens_df = heatmap %>% filter(event=="screens") %>%
  filter(cnt>0) %>% 
  group_by(STUDY_NUM)%>%
  summarise(min_screen = min(Date)) %>% 
  ungroup()

print(heatmap)

max_date_df = heatmap %>% 
  group_by(STUDY_NUM) %>%
  #summarise(max_date=min(as.Date(LAST_SUBJECT_SCREENED_DATE),
  #                    Sys.Date(),max(Date,na.rm=T),na.rm=T
  #       )
  #) %>%
  summarise(max_date = Sys.Date()) %>% 
  ungroup()


min_date = min_msl_df %>% full_join(min_cra_df) %>% full_join(min_screens_df) %>%
  group_by(STUDY_NUM) %>% 
  mutate(`Min Date` = max(min_msl,min_cra,min_screen,na.rm=T)) %>%
  select(STUDY_NUM,`Min Date`) %>% 
  ungroup()

swimlane_input = min_msl_df %>% 
  full_join(min_cra_df) %>% 
  full_join(min_screens_df) %>%
  full_join(max_date_df) %>% 
  group_by(STUDY_NUM) %>%
  mutate(min_date = min(min_msl,min_cra,min_screen,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = !c(STUDY_NUM,min_date),names_to = "Event",values_to = "Date")

min_date = min_msl_df %>% full_join(min_cra_df) %>% full_join(min_screens_df) %>%
  group_by(STUDY_NUM) %>% 
  mutate(`Min Date` = max(min_msl,min_cra,min_screen,na.rm=T)) %>%
  select(STUDY_NUM,`Min Date`) %>% 
  ungroup()

swimlane_input = min_msl_df %>% 
  full_join(min_cra_df) %>% 
  full_join(min_screens_df) %>%
  full_join(max_date_df) %>% 
  group_by(STUDY_NUM) %>%
  mutate(min_date = min(min_msl,min_cra,min_screen,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = !c(STUDY_NUM,min_date),names_to = "Event",values_to = "Date")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
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


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  #observe any changes to window size
  # observeEvent(input$window, {
  #   
  #   d <- jsonlite::fromJSON(input$window)
  #   print(d)
  #   output$winSize <- renderPrint({
  #     d
  #   })
  #   
  #   
  #   
  # })
  

  
  observeEvent(input[["study_ascent"]],
               {
                 print(2)
                 updateSelectInput(session = session,
                                   inputId = "study_ascent2",
                                   selected = input[["study_ascent"]])
               })
  
  observeEvent(input[["study_ascent2"]],
               {
                 updateSelectInput(session = session,
                                   inputId = "study_ascent",
                                   selected = input[["study_ascent2"]])
               })
  
  
  
  output$rs_plot <- renderPlotly({
    
    rs_df = glmm_recruitment %>% filter(Study %in% input$study_ascent2) %>%
      filter(regressor_type %in% input$topic_type_select) %>%
      #filter(Regressor %in% input$topic_select) %>%
      mutate(
        mult_lower_95 = exp(`X2.5..`),
        #mult_upper_75 = exp(avg_log_mult+1.15*se_mult),
        mult_upper_95 = exp(`X97.5..`)
        #mult_lower_75 = exp(avg_log_mult-1.15*se_mult),
      ) %>% filter(mult_upper_95 - Estimate < 10) %>% 
      rename(`Type`=regressor_type) %>% 
      mutate(Regressor = gsub("Recruitment Strategy","RS",Regressor))
    
    rs <- ggplot(data=rs_df, 
                 aes(y=Estimate,x=Regressor,#color=regressor_type,
                     group=Type,
                     text = paste0(
                       "Topic: ",Regressor,"<br>",
                       "Lower 95%: ", round(mult_lower_95,2),"<br>",
                       "Average Estimate: ", round(Estimate,2),"<br>",
                       "Upper 95%: ", round(mult_upper_95,2),"<br>"
                     )
                     )) +
      #geom_point()+
      geom_hline(aes(yintercept = 1))+
      facet_wrap(~Study,nrow=1)+
      geom_crossbar(aes(ymin=mult_lower_95,ymax=mult_upper_95,fill=Type
                    ),alpha=0.5,width=0.8,
                    fatten=1.5)+
      #scale_color_manual(values = c("Barrier" = barrier_red, "Recruitment Strategy" = rs_blue) ) +
      scale_fill_manual(values = c("Barrier" = barrier_red, "Recruitment Strategy" = rs_blue) ) +
      labs(title="Multiplicative Effect Size of Engagement on Screen Rate",
           y = "Multiplicative Effect on Screen Rate",
           x = "Recruitment Strategy/Barrier")+ 
      theme_hc() + 
      coord_flip()+
      theme(legend.title=element_blank())
    ggplotly(rs,height=800,width=1000,tooltip = c("text"))
  })
  
  output$heatmap_sw <- renderPlotly({
    
    #swimlane visualizing date of first screen, date of first CRA, MSL,
    #and end date, jitter symbols when they overlap
    swimlane = ggplot(data=swimlane_input %>% 
                        rename(Study = STUDY_NUM) %>% 
                        filter(Study %in% input$study_heatmap),aes(x=Date,y=Study)) + 
      geom_line() + geom_point(aes(shape=Event,fill=Event),
                              size = 2,
                               #position = "jitter"
                               ) + 
      scale_y_discrete(limits = rev)+ 
      theme_hc()
    
    
    ggplotly(swimlane,height = max(250,100*length(input$study_heatmap)))
  })
  
  output$heatmap_table <- renderDT({
    datatable(ascent_heatmap %>% filter(STUDY_NUM %in% input$study_heatmap) %>% 
                rename(Study = STUDY_NUM) %>% 
                select(Study, `Total CRA Engs`, `Total MSL Engs`, `Total Screens`,
                       `Min Date`, `Max Date`, `CRA Attribution Window`, 
                       `MSL Attribution Window`,`ASCENT Eligible`),
              options = list(dom='t')
              )
    
  })
  
  output$heatmap_lc <- renderPlotly({
    print("LC")
    #d <- jsonlite::fromJSON(input$window)
    #print(d$height)
    
    ts_input = heatmap %>%
      filter(STUDY_NUM %in% input$study_heatmap) %>%
      left_join(min_date) %>%
      mutate(month = factor(month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
      arrange(year,month)%>%
      mutate(month_year = paste(month,year)) %>%
      select(STUDY_NUM,Date,event,cnt,LAST_SUBJECT_SCREENED_DATE,`Min Date`,ASCENT_Study) %>%
      group_by(STUDY_NUM) %>%
      mutate(cnt = ifelse(is.na(cnt),0,cnt),
             max_date=min(as.Date(LAST_SUBJECT_SCREENED_DATE),
                          Sys.Date(),max(Date)
             )
      ) %>%
      mutate(max_cnt = max(cnt)) %>%
      ungroup() %>% 
      left_join(min_screens_df,by="STUDY_NUM") %>% 
      mutate(x_label = as.Date((as.integer(`Min Date`)+as.integer(max_date))/2)) %>% 
      #if not ASCENT eligible then Min Date = Max Date = NA because study is not eligible 
      mutate(
        `min_window` = `Min Date`,
        `max_window` = max_date,
        alpha_rect = if_else(ASCENT_Study == "ASCENT Eligible",0.2,0),
        label_rect = if_else(ASCENT_Study == "ASCENT Eligible","Analysis Window","")
        
      )%>%filter(event!="screens")
    
    
    h1 = ggplot(data=ts_input,aes(x=Date,y=cnt,color=event,group=event,
                                  text=paste0("Date: ",Date,"<br>",
                                              "Event: ",event,"<br>",
                                              "Count: ",cnt,"<br>")
                                  ))+
      geom_line()+
      geom_point()+
      #geom_segment(aes(x = `Min Date`, y = 0, xend = `Min Date`, yend = max_cnt*0.9),inherit.aes = F)+
      #geom_segment(aes(x = max_date, y = 0, xend = max_date, yend = max_cnt*0.9),inherit.aes = F)+
      geom_segment(aes(x = min_screen, y = 0, xend = min_screen, yend = max_cnt*0.9, text = paste0("Date: ", Date)),inherit.aes = F)+
      #geom_vline(aes(xintercept=as.numeric(max_date)),)+
      #geom_vline(aes(xintercept=as.numeric(`Min Date`)))+
      #geom_text(aes(max_date, max_cnt*0.95), label = "Max\nDate", show.legend = FALSE,inherit.aes = F,size=2)+
      geom_text(aes(min_screen, max_cnt*0.95), label = "FSS", show.legend = FALSE,inherit.aes = F,size=2)+
      geom_text(aes(x_label, max_cnt*1.05), label = ts_input$label_rect, show.legend = FALSE,size=2,inherit.aes = F)+
      facet_wrap(~STUDY_NUM,ncol=2,scales="free")+
      geom_rect(aes(xmin=`min_window`,xmax=max_window,ymin=0,ymax=max_cnt,text = paste0("Min Date: ",min_window,"<br>",
                                                                                        "Max Date: ",max_window,"<br>")),
                fill="lightgreen",colour="black",
                alpha=ts_input$alpha_rect)+
      ylab("Count")
    
    ggplotly(h1,tooltip = c("text"))
  })
  
  
  output$decay_plot <- renderPlotly({
    #DECAY visuals
    glmm_decay_results_long = glmm_decay_results %>% select(study,time,cra_lower,cra,cra_upper,msl_lower,msl,msl_upper) %>%
      pivot_longer(!c(study,time,cra_upper,cra_lower,msl_upper,msl_lower),names_to="lineby",values_to="value") %>% 
      mutate(lower = ifelse(lineby=="cra",cra_lower,msl_lower),
             upper = ifelse(lineby=="cra",cra_upper,msl_upper),
             colorby = if_else(lineby=="cra",cra_color,msl_color)
      ) %>%
      rename(Engagement=lineby)%>% 
      filter(study %in% input$study_ascent)
    
    if(!"CRA" %in% input$function_select){
      glmm_decay_results_long = glmm_decay_results_long %>% filter(Engagement != "cra")
    }
    if(!"MSL" %in% input$function_select){
      glmm_decay_results_long = glmm_decay_results_long %>% filter(Engagement != "msl")
    }
    
    auc_df = glmm_decay_results %>% 
      group_by(study) %>% 
      summarise(`cra` = round(sum(cra-1),2),
                `msl` = round(sum(msl-1),2)) %>% 
      pivot_longer(cols =  c(msl,cra),names_to = "Engagement",values_to="AUC")
    
    halflife_df = glmm_decay_results %>% 
      group_by(study) %>% 
      summarise(`cra` = round(log(2)/(-1*max(cra_k)),2),
                `msl` = round(log(2)/(-1*max(msl_k)),2)) %>% 
      pivot_longer(cols =  c(msl,cra),names_to = "Engagement",values_to="HalfLife")
    
    decay_plot = ggplot(data=glmm_decay_results_long %>% left_join(auc_df) %>% left_join(halflife_df)
                        
      )+
      geom_line(aes(x=time,y=value,color=Engagement
      )) + 
      geom_point(aes(x=time,y=value,color=Engagement,text = paste0("Engagement: ", Engagement,"<br>",
                                    "Days: ", time, "<br>",
                                    "Y (lower 95%): ", round(lower,2),"<br>",
                                    "Average Estimate: ", round(value,2),"<br>",
                                    "Y (upper 95%): ", round(upper,2),"<br>",
                                    "AUC: ",round(AUC,2),"<br>",
                                    "Half-Life:",round(HalfLife,2),"<br>"
      )),size=0.1)+
      geom_ribbon(aes(x=time,ymin=lower,ymax=upper,fill=Engagement),alpha=0.4) + 
      scale_fill_manual(values = c("cra" = cra_color, "msl" = msl_color))+
      scale_color_manual(values = c("cra" = cra_color, "msl" = msl_color) )+
      geom_hline(aes(yintercept=1))+
      facet_wrap(~ study,ncol = 2)+
      theme(
        panel.border=element_rect(colour="black",linewidth=1),
      )+
      theme_hc()+
      labs(x="Days after Engagement",y="Multiplicative Effect on Screen Rate")+
      ggtitle(label = "Decaying Multiplicative Effect of Engagements<br>on Screen Rate over Time")    
    gp = ggplotly(decay_plot,height = 400, tooltip=c("text")) %>% 
      layout(title = list(y = .95, xref = "plot"),
             margin = list(l = 25, t = 75)
      )
    
    for (i in seq_along(gp$x$data)) {
      # Is the layer the first entry of the group?
      is_first <- grepl("^\\(.*?,1\\)", gp$x$data[[i]]$name)
      # Extract the group identifier and assign it to the name and legendgroup arguments
      gp$x$data[[i]]$name <- gsub("^\\((.*?),\\d+\\)", "\\1", gp$x$data[[i]]$name)
      gp$x$data[[i]]$legendgroup <- gp$x$data[[i]]$name
      # Show the legend only for the first layer of the group 
      if (!is_first) gp$x$data[[i]]$showlegend <- FALSE
    }
    gp
  })
  
  
  output$window_plot <- renderPlotly({
    
    ##coef plot overall
    study_window_ls = input$study_ascent
    
    if(!"CRA" %in% input$function_select){
      glmm_window_results = glmm_window_results %>% filter(eng != "CRA Window")
    }
    if(!"MSL" %in% input$function_select){
      glmm_window_results = glmm_window_results %>% filter(eng != "MSL Window")
    }
    
    p1 <- ggplot(data=glmm_window_results %>% filter(country=="Overall") %>%
                   filter(study %in% study_window_ls) %>% 
                   mutate(
                     mult_upper_95 = exp(avg_log_mult+1.96*se_mult),
                     #mult_upper_75 = exp(avg_log_mult+1.15*se_mult),
                     mult_lower_95 = exp(avg_log_mult-1.96*se_mult),
                     #mult_lower_75 = exp(avg_log_mult-1.15*se_mult),
                     rownum = rank(study)
                   ), aes(y=avg_mult,x=c(eng) )) +
      #geom_point()+
      facet_wrap(~study,ncol=1,strip.position = "left")+
      geom_hline(aes(yintercept=1))+
      geom_crossbar(aes(ymin=mult_lower_95,ymax=mult_upper_95,fill=eng,
                        text = paste0("Engagement: ", eng, "<br>",
                                      "Lower 95%: ", round(mult_lower_95,2),"<br>",
                                      "Avg Multiplier: ", round(avg_mult,2), "<br>",
                                      "Upper 95%: ", round(mult_upper_95,2),"<br>"
                                      ) 
                          ),width=0.8,alpha=0.5)+
      scale_fill_manual(values = c("CRA Window" = cra_color, "MSL Window" = msl_color))+
      #scale_color_manual(values = c("CRA Window" = cra_color, "MSL Window" = msl_color) ) +
      labs(title="Multiplicative Effect Size of Engagement on Screen Rate",
           y = "Multiplicative Effect on Screen Rate",
           x = "Study (group)")+ 
      theme_hc()+
      # theme(axis.text.x=element_blank(),
      #       axis.title.x.left = element_blank(),
      #       axis.ticks.x.left = element_blank(),
      #       legend.title = element_blank()
      #)+
      coord_flip()
    ggplotly(p1,height=400,tooltip=c("text"))
    
  })
  
}

shinyApp(ui, server)

