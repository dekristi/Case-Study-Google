
library(tidyverse)
library(readxl)

member <- read_excel("C:/Users/Kristine/Case_Study/TripData_2020.12_V2.xlsx", 
                     sheet = "Member")

ggplot(data = member) + geom_line(mapping = aes(Date, Trips_on_Sunday))
