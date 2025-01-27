install.packages("vitae")
install.packages("tidytext")


library(tidyverse)
library(dplyr)
library(readxl)
library(tidytext)
library(vitae)

Member_Casual <- read_excel("C:/Users/Kristine/Case_Study/TripData_2020.12_V2.xlsx", 
                            sheet = "Mem_Cas")
View(Member_Casual)

#Ordering the levels of the factor with factor() to organize the weekdays in certain order (non alphabetical).

Member_Casual$Day_of_week <- factor(Member_Casual$Day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#CREATING BAR CHART TO SHOW THE MOST POPULAR WEEKDAYS FOR MEMBERS AND CASUAL

Member_Casual$Day_of_week <- factor(Member_Casual$Day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(Member_Casual, aes(Day_of_week, Number_of_trips, fill = Day_of_week)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = .5, size = 16, face = "bold")) +
    facet_wrap(~Member_casual) +
  scale_fill_manual(name = "Weekdays", values = c("purple", "lightgreen", "blue", "orange", "cyan", "violet","lightblue")) +
    ggtitle("Number of rides per day", subtitle = "Decemebr, 2020 - November, 2021") +
  xlab("Weekdays") + 
  ylab("Number of rides")
 
# RIDES DYNAMIC THROUGH THE YEAR

#Setting the order of the period:

Member_Casual$Date <- factor(Member_Casual$Date, levels = c("12.2020.", "01.2021.", "02.2021.", "03.2021.", "04.2021.", "05.2021.", "06.2021.", "07.2021.", "08.2021.", "09.2021.", "10.2021.", "11.2021."))

#Summarizing the number of trips by months and casual&member

Dec_m = sum(Member_Casual[which(Member_Casual$Date=='12.2020.' & Member_Casual$Member_casual=='member'), 1])
Jan_m = sum(Member_Casual[which(Member_Casual$Date=='01.2021.' & Member_Casual$Member_casual=='member'), 1])
Feb_m = sum(Member_Casual[which(Member_Casual$Date=='02.2021.' & Member_Casual$Member_casual=='member'), 1])
Mar_m = sum(Member_Casual[which(Member_Casual$Date=='03.2021.' & Member_Casual$Member_casual=='member'), 1])
Apr_m = sum(Member_Casual[which(Member_Casual$Date=='04.2021.' & Member_Casual$Member_casual=='member'), 1])
May_m = sum(Member_Casual[which(Member_Casual$Date=='05.2021.' & Member_Casual$Member_casual=='member'), 1])
Jun_m = sum(Member_Casual[which(Member_Casual$Date=='06.2021.' & Member_Casual$Member_casual=='member'), 1])
Jul_m = sum(Member_Casual[which(Member_Casual$Date=='07.2021.' & Member_Casual$Member_casual=='member'), 1])
Aug_m = sum(Member_Casual[which(Member_Casual$Date=='08.2021.' & Member_Casual$Member_casual=='member'), 1])
Sep_m = sum(Member_Casual[which(Member_Casual$Date=='09.2021.' & Member_Casual$Member_casual=='member'), 1])
Oct_m = sum(Member_Casual[which(Member_Casual$Date=='10.2021.' & Member_Casual$Member_casual=='member'), 1])
Nov_m = sum(Member_Casual[which(Member_Casual$Date=='11.2021.' & Member_Casual$Member_casual=='member'), 1])

Dec_c = sum(Member_Casual[which(Member_Casual$Date=='12.2020.' & Member_Casual$Member_casual=='casual'), 1])
Jan_c = sum(Member_Casual[which(Member_Casual$Date=='01.2021.' & Member_Casual$Member_casual=='casual'), 1])
Feb_c = sum(Member_Casual[which(Member_Casual$Date=='02.2021.' & Member_Casual$Member_casual=='casual'), 1])
Mar_c = sum(Member_Casual[which(Member_Casual$Date=='03.2021.' & Member_Casual$Member_casual=='casual'), 1])
Apr_c = sum(Member_Casual[which(Member_Casual$Date=='04.2021.' & Member_Casual$Member_casual=='casual'), 1])
May_c = sum(Member_Casual[which(Member_Casual$Date=='05.2021.' & Member_Casual$Member_casual=='casual'), 1])
Jun_c = sum(Member_Casual[which(Member_Casual$Date=='06.2021.' & Member_Casual$Member_casual=='casual'), 1])
Jul_c = sum(Member_Casual[which(Member_Casual$Date=='07.2021.' & Member_Casual$Member_casual=='casual'), 1])
Aug_c = sum(Member_Casual[which(Member_Casual$Date=='08.2021.' & Member_Casual$Member_casual=='casual'), 1])
Sep_c = sum(Member_Casual[which(Member_Casual$Date=='09.2021.' & Member_Casual$Member_casual=='casual'), 1])
Oct_c = sum(Member_Casual[which(Member_Casual$Date=='10.2021.' & Member_Casual$Member_casual=='casual'), 1])
Nov_c = sum(Member_Casual[which(Member_Casual$Date=='11.2021.' & Member_Casual$Member_casual=='casual'), 1])


# Creating new dataframe

df_trips_member <- data.frame(Date = rep(c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"),times=2),
                              trips_total = c(Dec_m, Jan_m, Feb_m, Mar_m, Apr_m, May_m, Jun_m, Jul_m, Aug_m, Sep_m, Oct_m, Nov_m, 
                                              Dec_c, Jan_c, Feb_c,Mar_c, Apr_c, May_c, Jun_c, Jul_c, Aug_c, Sep_c, Oct_c, Nov_c),
                              member_casual = rep(c("member", "casual"),each=12))

#View(df_trips_member)

# Creating bar chart: rides dynamic through the year

df_trips_member$Date <- factor(df_trips_member$Date, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))

ggplot(df_trips_member, aes(Date, trips_total, fill = member_casual)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(" ", values=c("lightgreen","lightblue")) +
  #to show numbers properly on y axis
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ',',
                                  decimal.mark = '.')) + 
  geom_text(aes(label = trips_total), vjust = 2.5, cex = 2.7) +
  facet_wrap(~member_casual) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold")) +
  ggtitle("Rides dynamic through the year") +
  xlab(" ") + 
  ylab("Number of rides")

#PIE CHART PREFERED RIDEABLE TYPE(MEMBERS)
#CREATING NEW DATA FRAME TO SUMNARIZE NUMBER OF TRIPS WITH DIFFERENT RIDEABLE TYPES FOR CATEGORY "MEMBER"

#calculating number of trips in column if two conditions are fulfilled: electric bike/member
elec_mem = sum(Member_Casual[which(Member_Casual$Bike=='electric_bike' & Member_Casual$Member_casual=='member'), 1]) 
class_mem = sum(Member_Casual[which(Member_Casual$Bike=='classic_bike' & Member_Casual$Member_casual=='member'), 1])
dock_mem = sum(Member_Casual[which(Member_Casual$Bike=='docked_bike' & Member_Casual$Member_casual=='member'), 1])

#creating new data frame
df_member <- data.frame(Rideable_type = c("electric_bike", "classic_bike", "docked_bike"),
                        trips_sum = c(elec_mem, class_mem, dock_mem))

view(df_member)

#creating pie chart
x <- df_member$trips_sum
y <- df_member$Rideable_type
pie_labels <- paste0(y, " = ", round(100 * x/sum(x), 2), "%") # to show values in %




pie3D(x,labels=pie_labels_c,explode=0.05, col = topo.colors(6), 
      labelcex = 1.2, border = "white", main="Preferred rideable (members)")

#PIE CHART PREFERED RIDEABLE TYPE(CASUAL)

elec_cas = sum(Member_Casual[which(Member_Casual$Bike=='electric_bike' & Member_Casual$Member_casual=='casual'), 1])
class_cas = sum(Member_Casual[which(Member_Casual$Bike=='classic_bike' & Member_Casual$Member_casual=='casual'), 1])
dock_cas = sum(Member_Casual[which(Member_Casual$Bike=='docked_bike' & Member_Casual$Member_casual=='casual'), 1])

df_casual <- data.frame(Rideable_type = c("electric_bike", "classic_bike", "docked_bike"),
                        trips_sum = c(elec_cas, class_cas, dock_cas))

view(df_casual)

xc <- df_casual$trips_sum
yc <- df_casual$Rideable_type
pie_labels_c <- paste0(yc, " = ", round(100 * xc/sum(xc), 2), "%")

pie(xc, labels = pie_labels_c, col = topo.colors(6), cex = .9, border = "white", main = "Preferred rideable type (casual)")

# 3D PIE

library(plotrix)
pie3D(xc,labels=pie_labels_c,explode=0.05, col = topo.colors(6), 
      labelcex = 1.2, border = "white", main="Prefered rideable (casual)")

#THE AVERAGE AND THE MAXIMUM LENGTH OF RIDES

# Creating new data frame to summarize the average length of ride by months/ member&casual

df_AVG <- data.frame(aggregate(x = Member_Casual$AVG_length_minutes,
                               by= list(Member_Casual$Date, Member_Casual$Member_casual),
                               FUN=mean))
View(df_AVG)

#Renaming the columns
names(df_AVG) <- c("Date", "member_casual", "AVG_length_min")
View(df_AVG)

install.packages("plotly")
library(plotly)
install.packages("hrbrthemes")
library(hrbrthemes)


ggplot(df_AVG, aes(Date, AVG_length_min, group=member_casual)) +
  geom_line(aes(linetype = member_casual))+
  geom_point(color="red") +
  guides(linetype=guide_legend(" ")) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle("Average length of ride in minutes") +
  xlab(" ") +
  ylab("Minutes") 

# THE MAX LENGTH OF RIDE IN HOURS
# Creating new data frame to identify the max length of ride by months/ member&casual

df_MAX <- data.frame(aggregate(x = Member_Casual$Max_length_hours,
                               by= list(Member_Casual$Date, Member_Casual$Member_casual),
                               FUN=max))
View(df_MAX)

#Renaming the columns
names(df_MAX) <- c("Date", "member_casual", "Max_length")
View(df_MAX)

ggplot(df_MAX, aes(Date, Max_length, group = member_casual)) +
  geom_line(aes(linetype = member_casual)) +
  geom_point(color="red") +
  guides(linetype = guide_legend(" ")) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = .5, hjust = 1)) +
  ggtitle("Maximum length of ride in hours") +
  xlab(" ") +
  ylab("Hours")





