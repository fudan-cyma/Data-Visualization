require(ggplot2)
require(dplyr)


data <- read.csv('AirlineDelayData2003-2017.csv', header = TRUE, sep = ',')

colnames(data)

filter(data, year == 2017 & month == 2)
