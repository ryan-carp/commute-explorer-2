################################################################################
# Server logic of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-13 09:21:18
################################################################################

library(gridExtra)

theme_dark_grey <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold", colour = '#ffffb3',
                                      size = rel(1.2), hjust = 0.5, margin = margin(0,0,20,0)),
            text = element_text(),
            panel.background = element_rect(colour = NA, fill = 'grey20'),
            plot.background = element_rect(colour = NA, fill = '#262626'),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1), colour = 'white'),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(colour = 'grey70'), 
            axis.line.x = element_line(colour="grey70"),
            axis.line.y = element_line(colour="grey70"),
            axis.ticks = element_line(colour="grey70"),
            panel.grid.major = element_line(colour="#262626"),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill ='#262626'),
            legend.text = element_text(color = 'white'),
            legend.key = element_rect(colour = NA, fill = '#262626'),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vetical",
            legend.key.size= unit(0.5, "cm"),
            #legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic", colour = 'white'),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#2D3A4C",fill="#2D3A4C"),
            strip.text = element_text(face="bold", colour = 'white')
    ))
}

server  <- function(input, output) {
  
  
  # Make plots
  output$line <-
    renderPlotly({
      ts_input = heatmap %>%
        filter(STUDY_NUM %in% input$studies) %>%
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
        #mutate(x_label = as.Date((as.integer(`Min Date`)+as.integer(max_date))/2)) %>% 
        #if not ASCENT eligible then Min Date = Max Date = NA because study is not eligible 
        mutate(
          `min_window` = `Min Date`,
          `max_window` = max_date,
          alpha_rect = if_else(ASCENT_Study == "ASCENT Eligible",0.2,0),
          label_rect = if_else(ASCENT_Study == "ASCENT Eligible","Analysis Window","")
          
        ) %>% 
        filter(event!="screens")
      
      
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
        #geom_text(aes(x_label, max_cnt*1.05), label = ts_input$label_rect, show.legend = FALSE,size=2,inherit.aes = F)+
        facet_wrap(~STUDY_NUM,ncol=2,scales="free")+
        geom_rect(aes(xmin=`min_window`,xmax=max_window,ymin=0,ymax=max_cnt,text = paste0("Min Date: ",min_window,"<br>",
                                                                                          "Max Date: ",max_window,"<br>")),
                  fill="lightgreen",colour="black",
                  alpha=ts_input$alpha_rect)+
        ylab("Count") + 
        theme_dark_grey()
      
      ggplotly(h1,tooltip = c("text"))
    })
  
  
  output$bar <- renderPlotly({
    
    ##coef plot overall
    study_window_ls = input$studies
    
    if(!"CRA" %in% input$crm_func){
      glmm_window_results = glmm_window_results %>% filter(eng != "CRA Window")
    }
    if(!"MSL" %in% input$crm_func){
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
      facet_wrap(~study,ncol=1,strip.position = "left") +
      geom_hline(aes(yintercept=1))+
      geom_crossbar(aes(ymin=mult_lower_95,ymax=mult_upper_95,fill=eng,
                        text = paste0("Engagement: ", eng, "<br>",
                                      "Lower 95%: ", round(mult_lower_95,2),"<br>",
                                      "Avg Multiplier: ", round(avg_mult,2), "<br>",
                                      "Upper 95%: ", round(mult_upper_95,2),"<br>"
                        ) 
      ),width=0.8,alpha=0.5)+
      scale_fill_manual(values = c("CRA Window" = "#4e79a7", "MSL Window" = "#d27dcf"))+
      #scale_color_manual(values = c("CRA Window" = cra_color, "MSL Window" = msl_color) ) +
      labs(title= " ",
           y = "Multiplicative Effect on Screen Rate",
           x = "Study (group)")+ 
      theme_hc()+
      # theme(axis.text.x=element_blank(),
      #       axis.title.x.left = element_blank(),
      #       axis.ticks.x.left = element_blank(),
      #       legend.title = element_blank()
      #)+
      coord_flip() + 
      theme_dark_grey()
    
    
    ggplotly(p1,tooltip=c("text")) }) 
  
  output$plot <- renderPlotly({ 
    
    rs_df = glmm_recruitment %>% filter(Study %in% input$studies) %>%
      filter(regressor_type %in% input$topics) %>%
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
      scale_fill_manual(values = c("Barrier" = "#e05759", "Recruitment Strategy" = "#4d78a5") ) +
      labs(title=" ",
           y = "Multiplicative Effect on Screen Rate",
           x = "Recruitment Strategy/Barrier")+ 
      theme_hc() + 
      coord_flip()+
      theme(legend.title=element_blank()) + 
      theme_dark_grey()
    
    ggplotly(rs,tooltip = c("text")) }) 
  
  
  output$last <- renderPlotly({
    #DECAY visuals
    glmm_decay_results_long = glmm_decay_results %>% select(study,time,cra_lower,cra,cra_upper,msl_lower,msl,msl_upper) %>%
      pivot_longer(!c(study,time,cra_upper,cra_lower,msl_upper,msl_lower),names_to="lineby",values_to="value") %>% 
      mutate(lower = ifelse(lineby=="cra",cra_lower,msl_lower),
             upper = ifelse(lineby=="cra",cra_upper,msl_upper),
             colorby = if_else(lineby=="cra","#4e79a7","#d27dcf")
      ) %>%
      rename(Engagement = lineby)%>% 
      filter(study %in% input$studies)
    
    if(!"CRA" %in% input$crm_func){
      glmm_decay_results_long = glmm_decay_results_long %>% filter(Engagement != "cra")
    }
    if(!"MSL" %in% input$crm_func){
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
      # theme(
      #   panel.border=element_rect(colour="black",linewidth=1),
      # )+
      theme_hc() + 
      theme_dark_grey() + 
      labs(x="Days after Engagement",y="Multiplicative Effect on Screen Rate")+
      ggtitle(label = " ")    
    
    
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
    gp }) 
  
}
