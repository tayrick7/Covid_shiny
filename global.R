library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(plotly)
library(maps)
covid_full <- read.csv("owid-covid-data.csv",
                       stringsAsFactors = FALSE,
                       check.names =  FALSE)

covid_full$date <- as.Date(covid_full$date)

countries = c("United States", "India", "Brazil", "France", "United Kingdom", "Russia", 
              "Turkey", "Italy", "Germany", "Spain", "Argentina",
              "Iran", "Colombia", "Poland", 'Mexico', "Netherlands", 
              "Indonesia", "Ukraine", "South Africa",
              "Philippines")

countries <- sort(countries)

covid <- covid_full[covid_full$location %in% countries, ]
covid <- covid[(covid$date >= "2021-01-01" & covid$date <= "2021-12-31"), ]

cleand_covid = data.frame()
for ( j in countries){
  single <- covid %>% dplyr::filter(location == j)
  
  
  country <- c(j)
  time <- c(single[1,]$date)
  stringency <- c(single[1,]$stringency_index)
  total_new_cases_per_million <- c()
  total_new <- single[1,]$new_cases_per_million
  for( i in 1:nrow(single)){
    temp_index <- single[i,]$stringency_index
    if(!is.na(temp_index)){
      if (temp_index != stringency[length(stringency)] ){
        country <- append(country,single[i,]$location)
        time <- append(time,single[i,]$date)
        stringency <- append(stringency,single[i,]$stringency_index)
        total_new_cases_per_million <- append(total_new_cases_per_million,total_new)
        total_new <- single[i,]$new_cases_per_million
        if (i == nrow(single)){
          total_new_cases_per_million <- append(total_new_cases_per_million,total_new)
        }
        
      } else{
        total_new <- total_new + single[i,]$new_cases_per_million
        if (i == nrow(single)){
          total_new_cases_per_million <- append(total_new_cases_per_million,total_new)
        }
      }
      
    } else{
      next
    }
    
  }
  
  single_cleaned <- data.frame(country,time,stringency,total_new_cases_per_million)
  cleand_covid <- rbind(cleand_covid, single_cleaned)
}



cleand_covid$stringency_difference <- 0

for (i in countries){
  index <- which ( cleand_covid$country == i )
  this_country <- cleand_covid[ cleand_covid$country == i , ]
  difference <- diff( this_country$stringency)
  cleand_covid[index, ]$stringency_difference <- c(0,   difference)
}

cleand_covid$new_cases_difference  <- 0

for (i in countries){
  index <- which ( cleand_covid$country == i )
  this_country <- cleand_covid[ cleand_covid$country == i , ]
  difference <- diff( this_country$total_new_cases_per_million)
  cleand_covid[index, ]$new_cases_difference  <- c(0, difference)
}


cleand_covid$total_new_cases_per_million<-cleand_covid$total_new_cases_per_million/100
total_cluster <- ggplot(data = cleand_covid) + geom_line(aes(x = time, y = total_new_cases_per_million, color = "total_new_cases_per_million"))+ geom_line(aes(x = time, y = stringency, color = "stringency")) +scale_x_date(breaks = "1 month",labels=date_format("%m"))+ theme(axis.text.x = element_text(angle = 60, hjust = 1))+ xlab("Month") + facet_wrap( ~ country, scale = "free_y")


world_map <- map_data("world2")

lag_data <- data.frame()

id <- 1
for (i in 1:length(countries)) {
  lag_location = countries[i]
  lag_countries <- cleand_covid %>% dplyr::filter(country == lag_location)
  start <- NULL
  start_date = NULL
  end_date = NULL
  max_lag_difftime = 0
  j = 1
  while (j < nrow(lag_countries)){
    temp_index <- lag_countries[j,]$stringency
    if(!is.na(temp_index)){
      if( j != nrow(lag_countries)){
        if (lag_countries[j,]$stringency < lag_countries[j+1,]$stringency) {
          start_date = lag_countries[j+1,]$time
          for (k in j+1:nrow(lag_countries)-1) {
            temp_case <- lag_countries[k,]$total_new_cases_per_million 
            temp_case_next <- lag_countries[k+1,]$total_new_cases_per_million
            if(!is.na(temp_case) && !is.na(temp_case_next) && length(temp_case) != 0 && length(temp_case_next) != 0){
              
              if (lag_countries[k,]$total_new_cases_per_million > lag_countries[k+1,]$total_new_cases_per_million) {
                end_date = lag_countries[k+1,]$time
                lag_difftime = difftime(end_date, start_date, units = "days")
                if (lag_difftime >= max_lag_difftime) {
                  max_lag_difftime = lag_difftime
                  start <- start_date
                }
                
                start_date = end_date
                j = k
                break
                
              }
            } else{
              break
            }
            
          }
          j = j + 1
        }
        
      }else{
        break
      }
    }else{
      break
    }
    j = j + 1
    
    
  }
  
  sub_lag_data <- data.frame(id,countries[i], max_lag_difftime, start)
  lag_data <- rbind(lag_data, sub_lag_data)
  id <- id + 1
  
}
names(lag_data)[2] <- "Country"

lag_data <- lag_data[order(lag_data$max_lag_difftime),]

word_geo <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")

world_merge<- merge(word_geo, lag_data, 
                    by.x = "COUNTRY", by.y = "Country",
                    all.x = TRUE)

fig <- plot_ly(world_merge, type='choropleth', locations=world_merge$CODE, z=world_merge$max_lag_difftime, text=world_merge$COUNTRY, colorscale="RdBu")%>%layout(title = 'COVID19: The max lag time in different countries', font=t, plot_bgcolor = "#e5ecf6")





