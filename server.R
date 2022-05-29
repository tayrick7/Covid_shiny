library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(plotly)
library(maps)

server <- function(input, output) {
  output$fd <-renderPlotly({
    if (length(input$death_most) == 0 & length(input$death_fewest) == 0 & length(input$most_strengthened) == 0 & length(input$least_strengthened) == 0) {
      g_repro_diff <- ggplot(cleand_covid %>% filter(country == input$country,time <= input$daterange[2],
                                                     time >= input$daterange[1]), aes(x = time, y = stringency_difference, 
                                                                                      group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("Stringency Index differences") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(g_repro_diff)
    }  else if (length(input$death_most) != 0)  {
      g_repro_diff_1 <- ggplot(cleand_covid %>% filter(country %in% input$death_most,time <= input$daterange[2],
                                                       time >= input$daterange[1]), aes(x = time, y = stringency_difference, 
                                                                                        group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("Stringency Index differences") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(g_repro_diff_1)
    } else if (length(input$death_fewest) != 0)  {
      g_repro_diff_2 <- ggplot(cleand_covid %>% filter(country %in% input$death_fewest,time <= input$daterange[2],
                                                       time >= input$daterange[1]), aes(x = time, y = stringency_difference, 
                                                                                        group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("Stringency Index differences") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(g_repro_diff_2)
    } else if (length(input$most_strengthened) != 0)  {
      g_repro_diff_3 <- ggplot(cleand_covid %>% filter(country %in% input$most_strengthened,time <= input$daterange[2],
                                                       time >= input$daterange[1]), aes(x = time, y = stringency_difference, 
                                                                                        group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("Stringency Index differences") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(g_repro_diff_3)
    } else if (length(input$least_strengthened) != 0)  {
      g_repro_diff_4 <- ggplot(cleand_covid %>% filter(country %in% input$least_strengthened,time <= input$daterange[2],
                                                       time >= input$daterange[1]), aes(x = time, y = stringency_difference, 
                                                                                        group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("Stringency Index differences") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(g_repro_diff_4)
    } 
  })
  
  output$fd_2 <-renderPlotly({
    if (length(input$death_most) == 0 & length(input$death_fewest) == 0 & length(input$most_strengthened) == 0 & length(input$least_strengthened) == 0) {
      g_repro_diff_nc <- ggplot(cleand_covid %>% filter(country == input$country,time <= input$daterange[2],
                                                        time >= input$daterange[1]), aes(x = time, y = new_cases_difference , 
                                                                                         group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("New cases per million differences") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region") 
      ggplotly(g_repro_diff_nc)
    } else if (length(input$death_most) != 0)  {
      g_repro_diff_nc_1 <- ggplot(cleand_covid %>% filter(country %in% input$death_most,time <= input$daterange[2],
                                                          time >= input$daterange[1]), aes(x = time, y = new_cases_difference , 
                                                                                           group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("New cases per million differences") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region") 
      ggplotly(g_repro_diff_nc_1)
    } else if (length(input$death_fewest) != 0)  {
      g_repro_diff_nc_2 <- ggplot(cleand_covid %>% filter(country %in% input$death_fewest,time <= input$daterange[2],
                                                          time >= input$daterange[1]), aes(x = time, y = new_cases_difference , 
                                                                                           group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("New cases per million differences") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region") 
      ggplotly(g_repro_diff_nc_2)
    } else if (length(input$most_strengthened) != 0)  {
      g_repro_diff_nc_3 <- ggplot(cleand_covid %>% filter(country %in% input$most_strengthened,time <= input$daterange[2],
                                                          time >= input$daterange[1]), aes(x = time, y = new_cases_difference , 
                                                                                           group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("New cases per million differences") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region") 
      ggplotly(g_repro_diff_nc_3)
    } else if (length(input$least_strengthened) != 0)  {
      g_repro_diff_nc_4 <- ggplot(cleand_covid %>% filter(country %in% input$least_strengthened,time <= input$daterange[2],
                                                          time >= input$daterange[1]), aes(x = time, y = new_cases_difference , 
                                                                                           group = country, color = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("New cases per million differences") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region") 
      ggplotly(g_repro_diff_nc_4)
    }
  })
  output$new_case <-renderPlotly({
    if (length(input$death_most) == 0 & length(input$death_fewest) == 0 & length(input$most_strengthened) == 0 & length(input$least_strengthened) == 0) {
      new <- ggplot(cleand_covid %>% filter(country == input$country,time <= input$daterange[2],
                                            time >= input$daterange[1]), aes(x = time, y = total_new_cases_per_million,color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("total new cases per million") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(new)
    } else if (length(input$death_most) != 0)  {
      new_1 <- ggplot(cleand_covid %>% filter(country %in% input$death_most,time <= input$daterange[2],
                                              time >= input$daterange[1]), aes(x = time, y = total_new_cases_per_million,color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("total new cases per million") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(new_1)
    } else if (length(input$death_fewest) != 0)  {
      new_2 <- ggplot(cleand_covid %>% filter(country %in% input$death_fewest,time <= input$daterange[2],
                                              time >= input$daterange[1]), aes(x = time, y = total_new_cases_per_million,color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("total new cases per million") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(new_2)
    } else if (length(input$most_strengthened) != 0)  {
      new_3 <- ggplot(cleand_covid %>% filter(country %in% input$most_strengthened,time <= input$daterange[2],
                                              time >= input$daterange[1]), aes(x = time, y = total_new_cases_per_million,color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("total new cases per million") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(new_3)
    } else if (length(input$least_strengthened) != 0)  {
      new_4 <- ggplot(cleand_covid %>% filter(country %in% input$least_strengthened,time <= input$daterange[2],
                                              time >= input$daterange[1]), aes(x = time, y = total_new_cases_per_million,color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("total new cases per million") +
        scale_y_continuous(labels = scales::comma)+
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(new_4)
    }
  })
  output$stringency <-renderPlotly({
    if (length(input$death_most) == 0 & length(input$death_fewest) == 0 & length(input$most_strengthened) == 0 & length(input$least_strengthened) == 0) {
      index <- ggplot(cleand_covid %>% filter(country == input$country,time <= input$daterange[2],
                                              time >= input$daterange[1]), aes(x = time, y = stringency, color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("stringency index") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(index)
    } else if (length(input$death_most) != 0)  {
      index_1 <- ggplot(cleand_covid %>% filter(country %in% input$death_most,time <= input$daterange[2],
                                                time >= input$daterange[1]), aes(x = time, y = stringency, color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("stringency index") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(index_1)
    } else if (length(input$death_fewest) != 0)  {
      index_2 <- ggplot(cleand_covid %>% filter(country %in% input$death_fewest,time <= input$daterange[2],
                                                time >= input$daterange[1]), aes(x = time, y = stringency, color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("stringency index") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(index_2)
    } else if (length(input$most_strengthened) != 0)  {
      index_3 <- ggplot(cleand_covid %>% filter(country %in% input$most_strengthened,time <= input$daterange[2],
                                                time >= input$daterange[1]), aes(x = time, y = stringency, color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("stringency index") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(index_3)
    } else if (length(input$least_strengthened) != 0)  {
      index_4 <- ggplot(cleand_covid %>% filter(country %in% input$least_strengthened,time <= input$daterange[2],
                                                time >= input$daterange[1]), aes(x = time, y = stringency, color = country, group = country)) +
        geom_line(lwd = 0.5) +
        theme_bw() +
        ylab("stringency index") +
        xlab("Month")+
        scale_x_date(breaks = "1 month",labels=date_format("%Y- %m"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = "Country/Region")
      ggplotly(index_4)
    }
  })
  
  output$cluster <-renderPlot({
    total_cluster
  })
  
  output$World_map_cases <- renderPlotly({
    fig
  })
  
  output$info <- renderText({ 
    "Disclamier:"
  })
  
  output$Info <- renderText({ 
    "All data used in this app come from third party and the purpose of this app is for communication and learning."
  })
  
  output$data <- renderText({ 
    "Data Source:"
  })
  
  output$Data <- renderText({ 
    "Data used in this app comes from Our World in Data."
  })
  
  output$ai <- renderText({ 
    "Additional information:"
  })
  
  output$Ai_1 <- renderText({ 
    "The first three tabs(except this one) are all interactive and users can set specific countries/regions and time range."
  })
  
  output$Ai_2 <- renderText({ 
    "World Map is only interactive and the user cannot make a selection. The last section is a combined plot and it has no special attribute."
  })
  
  output$Ai_3 <- renderText({ 
    "This shiny app was designed and created by group Covid-A10 from Data3888, University of Sydney."
  })
  
  output$name <- renderText({ 
    "Covid-A10 group member:"
  })
  
  output$name_1 <- renderText({ 
    "Kang Fu"
  })
  
  output$name_2 <- renderText({ 
    "Katherine An"
  })
  
  output$name_3 <- renderText({ 
    "Yachao Zhang"
  })
  
  output$name_4 <- renderText({ 
    "Yuxuan Qin"
  })
  
  output$name_5 <- renderText({ 
    "Yunshuo Zhang"
  })
}



