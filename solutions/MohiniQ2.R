flights = read.csv(("../data/ABIA.csv"))
head(flights)
summary(flights)
attach(flights)
library(mosaic)
library(tidyverse)
library(dplyr)
library(lubridate)

flights$date <- as.Date(with(flights, paste(Year, Month, DayofMonth,sep="-")), "%Y-%m-%d")
flights$FlightNum = as.factor(flights$FlightNum)
flights$Month = as.factor(flights$Month)
flights$DayofMonth = as.factor(flights$DayofMonth)
flights$Cancelled = as.factor(flights$Cancelled)
flights$Diverted = as.factor(flights$Diverted)
#cols = c("DayOfWeek", "DepTime", "CRSDepTime", "ArrTime", "CRSArrTime", "Cancelled", "Diverted")
#flights[, cols] = data.frame(apply(flights[, cols], 2, as.factor))

# Frequency every month
ggplot(data=flights)+
  geom_bar(aes(x=Month, stat='count'), fill = 'Light Blue')

# Frequency every day in a certain month
d1 = flights %>%
  filter(Month == "1") %>%
  group_by(DayOfWeek) %>%
  summarize(flightsPerDay = count(Year)/4)

ggplot(data=d1)+
  geom_bar(mapping = aes(x=DayOfWeek, y=flightsPerDay), stat = 'identity', fill = 'Light Blue')

# Cancelled flights
d2 = flights %>%
  group_by(CancellationCode) %>%
  summarize(CancelledCount = count(FlightNum))

ggplot(data = d2) +
  geom_bar(mapping = aes(x=CancellationCode, y = CancelledCount), stat = 'identity', fill = 'Light Blue')

d3 = flights %>%
  group_by(Cancelled, CancellationCode) %>%
  summarize(CancelledCount = count(FlightNum))

# Cancellation codes
ggplot(data = d3) +
  geom_bar(mapping = aes(x=Cancelled, y = CancelledCount), stat = 'identity', fill = 'Light Blue')+
  facet_grid(~CancellationCode)


# Diverted flights
d4 = flights %>%
  group_by(Diverted) %>%
  summarize(DivertedCount = count(FlightNum))

ggplot(data = d4) +
  geom_bar(mapping = aes(x=Diverted, y = DivertedCount), stat = 'identity', fill = 'Light Blue')

# Dep delay --> Airtime?
ggplot(data = flights) + 
  geom_point(mapping = aes(x=DepDelay, y = AirTime))

# More airtime --> arr delay?
ggplot(data = flights) + 
  geom_point(mapping = aes(x=AirTime, y = ArrDelay))

# Taxi out vs dep delay?
ggplot(data = flights) + 
  geom_point(mapping = aes(x=TaxiOut, y = DepDelay))

# Arr time vs Taxi in?
ggplot(data = flights) + 
  geom_point(mapping = aes(x=ArrDelay, y = TaxiIn))
