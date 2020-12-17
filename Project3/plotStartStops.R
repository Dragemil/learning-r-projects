  # Libraries
  library(ggplot2)
  library(dplyr)
  library(plotly)
  library(ggmap)
  library(forcats)
  
  source("registerGoogle.R") # uwierzytelnienie w GCP przy pomocy klucza  
  source("data.R")
  
  jc <- c(lon = -74.05, lat = 40.72)
  jc_map <- get_map(location = jc, zoom = 14)
  
  plotlyStart <- function(hourStart, hourEnd, month) {
    bikes <- aggregatedBikes(hourStart, hourEnd, month) %>%
      mutate(text=paste("Start station:", station.name, "\nBikes difference:", difference)) %>%
      mutate(status=if(difference>0) "+" else if(difference==0) "0" else "-") %>%
      rename(
        lon = station.longitude,
        lat = station.latitude
      )
    
    map <- ggmap(jc_map, extent="device") + 
      geom_point(
        data=bikes, 
        alpha=0.5, 
        aes(x=lon, 
            y=lat, 
            size=abs(difference), 
            text=text, 
            colour=status)) +
      scale_color_manual(name = "status",
                         values = c("-" = "brown",
                                    "+" = "darkgreen", 
                                    "0" = "orange")) +
      scale_size(range = c(.01, log(max(abs(bikes$difference)), base=2)) + 1, 
                 name="Bike rentals") + 
      theme(legend.title=element_blank())
    
    # turn ggplot interactive with plotly
    ggplotly(map, tooltip="text")
  }
  
  plotlyTopStations <- function(hourStart, hourEnd, month) {
    mostFrequentStations(hourStart, hourEnd, month) %>%
      mutate(station.name = fct_reorder(station.name, count)) %>%
      ggplot(aes(y=station.name, x=count, fill=as.factor(count), text=paste("Station:", station.name, "\nTotal traffic:", count))) + 
      geom_bar(stat = "identity") +
      theme(legend.position="none") -> plot
      # turn ggplot interactive with plotly
      ggplotly(plot, tooltip = "text")
  }
  
  plotlyFreeriders <- function(hourStart, hourEnd, month) {
    freeriders(hourStart, hourEnd, month) %>%
    mutate(dayOfTheWeek = fct_reorder(dayOfTheWeek, c(9,10,1,2,11,12,13,14,7,8,3,4,5,6))) %>%
    ggplot(aes(fill=usertype, y=fraction, x=dayOfTheWeek, text=paste("User type:", usertype, "\nDay of the week:", dayOfTheWeek, "\nFraction:", format(round(fraction, 2), nsmall = 2), "%"))) + 
      geom_bar(position="dodge", stat="identity") +
      ggtitle("Freeriders percentage") -> plot
    
    ggplotly(plot, tooltip = "text")
  }
  
