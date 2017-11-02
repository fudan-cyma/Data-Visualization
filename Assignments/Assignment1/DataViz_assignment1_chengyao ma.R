# Load the packages needed 
require(ggplot2)  
require(dplyr)
require(scales)
require(reshape2)
require(maps)



#Load the data
setwd('C:/Users/cyma9/dropbox/dataviz/assignment 1')   

data <- read.csv('AirlineDelayData2003-2017.csv', header = TRUE, sep = ',')




#Recode the variable Season.
data$season = recode(data$month, `1` = 'winter', `2` = 'winter',`3` = 'spring',`4` = 'spring',`5` = 'spring', `6` = 'summer', `7` = 'summer',`8` = 'summer', `9` = 'fall', `10` = 'fall', `11` = 'fall', `12` = 'winter')  
data$season <- factor(data$season, labels =  c('spring','summer','fall','winter'),levels = c('spring','summer','fall','winter'))



#-=--------------------------------Plot 1 ----------------------------------------------------------------
filter(data, year >= 2011) %>% group_by(year,season) %>% summarize(delay = sum(arr_del15, na.rm = TRUE)/sum(arr_flights, na.rm = TRUE) )  %>%
  ggplot(aes(x = year, y = delay, color = season)) +  
  geom_line(size = 1.5) +
  scale_y_continuous(name = "Delay Rate", labels = scales::percent, breaks = seq(0.1,0.25,0.05),limits=c(0.1, 0.25)) +
  scale_color_manual(values = c('#C8E6C9','#F44336','#FFECB3','#E3F2FD')) +
  labs(title = 'Delay Rate by Season (2012-2017)', caption = 'Data of 2017 Fall is currently unavailable.') + 
  theme(
        plot.title = element_text(size = rel(1)), legend.position = c(.9,.2), legend.background = element_blank(),
        legend.key = element_blank(),         
        panel.background = element_rect(fill = "white", colour = "grey50")

  )

#----------------------------------Plot 2 ----------------------------------------------------------------
#Summarize reason of delay for different seasons
reason <-filter(data, year >= 2012) %>%   ## Plot 2 
  group_by(season) %>% 
  summarize(
         Weather = sum( weather_ct, na.rm = TRUE) / sum(arr_del15, na.rm = TRUE),
         Carrier = sum( carrier_ct, na.rm = TRUE) / sum(arr_del15, na.rm = TRUE),
         NAS = sum( nas_ct, na.rm = TRUE) / sum(arr_del15, na.rm = TRUE),
         Secruity = sum( security_ct, na.rm = TRUE) / sum(arr_del15, na.rm = TRUE),
         Late_Aircraft = sum( late_aircraft_ct, na.rm = TRUE) / sum(arr_del15, na.rm = TRUE))

reason_time <-filter(data, year >= 2012) %>%   ## Plot 2 
  group_by(season) %>% 
  summarize(
         Weather = sum( weather_delay, na.rm = TRUE) / sum( arr_delay, na.rm = TRUE),
         Carrier = sum( carrier_delay, na.rm = TRUE) / sum( arr_delay, na.rm = TRUE),
         NAS = sum( nas_delay, na.rm = TRUE) / sum( arr_delay, na.rm = TRUE),
         Secruity = sum( security_delay, na.rm = TRUE) / sum( arr_delay, na.rm = TRUE),
         Late_Aircraft = sum( late_aircraft_delay, na.rm = TRUE) / sum( arr_delay, na.rm = TRUE))

reason$type<-'Frequency'
reason_time$type<-'Time'
reason_full <- rbind(reason, reason_time)
reason_long <- melt(reason_full)

ggplot(data = reason_long,aes(x = season, y = value, fill = variable)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(palette = 'Pastel1') +
  scale_y_continuous(name = "Percentage", labels = scales::percent) +
  facet_grid(~type) +
  labs(title = 'Delay Reason by Summer(since 2012)') +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    plot.title = element_text(size = rel(1)),
    legend.position = 'bottom'
  )




#----------------------------------Plot 3 ----------------------------------------------------------------


#Calculate delay rate by carriers 
carrier <- data %>%
  filter(year >= 2012, season == 'summer') %>%
  group_by(carrier) %>%
  summarize(summer_delay = sum(arr_del15, na.rm = TRUE)/sum(arr_flights, na.rm = TRUE)) 


#Order carriers by delay rate in Summer
carrier <- arrange(carrier, desc(summer_delay))


ggplot(carrier, aes(x = reorder(carrier,summer_delay), y = summer_delay, fill = summer_delay > 0.25)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(name = "Delay Rate", labels = scales::percent) +
  xlab('Carrier') + 
  scale_fill_manual(values = c('#AED581', '#FF0000'), guide = FALSE) +
  labs(title = 'Delay Rate in Summer of US Carriers(since 2012)') + 
  theme(
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(size = rel(1))
  )





#---------------------------------Plot 4----------------------------------------------------------------


#Sort top 30 airports by total flights
airport_top50 <- filter(data, year >= 2012)  %>% group_by(airport) %>%
  summarize( sumflights = sum(arr_flights, na.rm = TRUE)) %>%
  top_n(30)

#Calculate delay rate in summer of every airport

airport <- data %>%
  filter(year >= 2012, season == 'summer', airport %in% airport_top50$airport) %>%
  group_by(airport) %>%
  summarize(total_flights = sum(arr_flights),summer_delay = sum(arr_del15, na.rm = TRUE)/sum(arr_flights, na.rm = TRUE)) 


#Load the geographic information of every airport
airport_loc <- read.table('airports.dat',header = FALSE,sep = ',')
airport <- arrange(airport, desc(summer_delay))

all_states <- map_data("state")
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="grey10", fill="white" )
airport_loc <- read.table('airports.dat',header = FALSE,sep = ',')
airport_loc <- airport_loc[c(5,7,8)]
names(airport_loc)[1] <- 'airport'
names(airport_loc)[2] <- 'la'
names(airport_loc)[3] <- 'long'

# Exclude airport of Honolulu and Puerto Rico to keep the map tidy 
airport_merge <- merge(airport, airport_loc, by = 'airport' )
airport_merge <- filter(airport_merge,  airport !='HNL')
airport_merge <- filter(airport_merge,  airport !='SJU')



ggplot() + 
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey90" ) +
  geom_point( data = airport_merge, aes(x=long, y=la, size =total_flights, color=summer_delay)) + 
  scale_colour_gradient(low = 'white', high = '#FF0000') + 
  guides(fill = FALSE) +
  labs(title = 'Delay Rate in Summer of US TOP30 Airports(since 2012)', caption = 'Data source of airport location: https://openflights.org/data.html') + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(size = rel(1))
        )




