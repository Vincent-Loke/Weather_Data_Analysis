#------------------------------------------------------------------------------#
#                             Name: Loke Weng Khay                             #
#------------------------------------------------------------------------------#

#Data import 
filelocation="C:\\Users\\Asus Notebook\\Documents\\weather.csv"
weather = read.csv(filelocation,header = TRUE)

#Install and load library
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("grid")
library(grid)
install.packages("RColorBrewer")
library(RColorBrewer)

#Explore Dataset
head(weather)
tail(weather)
nrow(weather)
ncol(weather)
summary(weather)
str(weather)

#---------------------------Data Pre-Processing--------------------------------
#view all missing data 
colSums(is.na(weather))


#insert missing data for Sunshine
Sunshine_Missing_Data = function()
{

  location1 = which(is.na(weather$Sunshine))
  for(i in 1:length(location1))
  {
    Evaporation_Rate = weather$Evaporation[location1[i]]
    value = sapply(split(weather$Sunshine, 
                         weather$Evaporation==Evaporation_Rate), 
                   mean,na.rm=TRUE)
    weather$Sunshine[location1[i]]=value[2]
    i=i+1
  }
  weather<<-weather
}

Sunshine_Missing_Data()
colSums(is.na(weather))

#insert missing data for WindDir9am
WindDir9am_Missing_Data = function()
{
  location2 = which(is.na(weather$WindDir9am))
  for(i in 1:length(location2))
  {
    windSpeedat9am = weather$WindSpeed9am[location2[i]]
    if(is.na(windSpeedat9am))
    {
      i=i+1
    }else
    {
      weather$WindDir9am[location2[i]]="No Wind"
      i=i+1
    }
  }
  weather<<-weather
}
WindDir9am_Missing_Data()
colSums(is.na(weather))

#insert missing data for WindDir3pm
WindDir3pm_Missing_Data = function()
{
  
  location3 = which(is.na(weather$WindDir3pm))
  for(i in 1:length(location3))
  {
    windSpeedat3pm = weather$WindSpeed3pm[location3[i]]
    if(is.na(windSpeedat3pm))
    {
      i=i+1
    }else
    {
      weather$WindDir3pm[location3[i]]="No Wind"
      i=i+1
    }
  }
  weather<<-weather
}
WindDir3pm_Missing_Data()
colSums(is.na(weather))

#remove missing data for WindDir9am And Windspeed9am
WindDir9am_Remove_Data = function()
{
  i=0
  location4 = which(is.na(weather$WindDir9am))
  for(i in 1:length(location4))
  {
    location5 = which(is.na(weather$WindDir9am))
    windDirat9am = weather$WindDir9am[location5[1]]
    if(is.na(windDirat9am))
    {
      weather = weather[-location5[1], ]
      i=i+1
    }
  }
  weather<<-weather
}
WindDir9am_Remove_Data()
colSums(is.na(weather))

#remove missing data for WindGustDir and WindGustSpeed
WindGustDir_Remove_Data = function()
{
  i=0
  location6 = which(is.na(weather$WindGustDir))
  for(i in 1:length(location6))
  {
    location7 = which(is.na(weather$WindGustDir))
    windDirat9am = weather$WindGustDir[location7[1]]
    if(is.na(windDirat9am))
    {
      weather = weather[-location7[1], ]
      i=i+1
    }
  }
  weather<<-weather
}
WindGustDir_Remove_Data()
colSums(is.na(weather))

#--------------------------------Extra Function--------------------------------#

#Extra Function 1
#Convert Wind Direction into Wind Degree
dir <- setNames(seq(0, 360 , by=22.5),
                c("N","NNE","NE","ENE", 
                  "E","ESE","SE","SSE",
                  "S","SSW","SW","WSW",
                  "W","WNW","NW","NNW"))



#Extra Function 2
#R Code to create a Compass Rose graph
plot.windrose <- function(data,spd,dir,spdres=2,dirres=22.5,spdmin=2,spdmax=20,
                        spdseq=NULL,palette="YlGnBu",countmax=NA,debug=0,title2)
{
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd=spd,dir=dir)
    spd = "spd"
    dir = "dir"
  }else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,n.colors.in.range),
                                                min(9,n.colors.in.range)),
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),'-',c(spdseq[2:n.spd.seq])),
                    paste(spdmax,"-",max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  }else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),'-',c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],breaks = spd.breaks,
                         labels = spd.labels,ordered_result = TRUE)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2,360-3*dirres/2,by = dirres),"-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],breaks = dir.breaks,ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
    
  }  
  
  # create the plot ----
  p.windrose <- ggplot(data = data,aes(x = dir.binned,fill = spd.binned,
                                       y = (..count..)/sum(..count..)))+
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","NNE","NE","ENE", 
                                "E","ESE","SE","SSE",
                                "S","SSW","SW","WSW",
                                "W","WNW","NW","NNW")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (km/h)", 
                      values = spd.colors,
                      drop = FALSE) +
    labs(title = title2) + 
    theme(axis.title.x = element_blank()) + 
    scale_y_continuous(labels = scales::percent ) +
    ylab("Frequency")
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}


#----------------------------------Question 1-----------------------------------
#Question 1: Is the United States currently an excellent time to travel to? 

#Analysis 1.1: Find the Season in the United States of America?
#Analysis 1.1 (Minimum and Maximum Temperature)
par(mar = c(10,4,4,2) + 0.1)
sdata1 = (summary(weather$MinTemp))
summaryStat1 = paste(names(sdata1),format(sdata1,digit=2),collapse = ", ")
sdata2 = (summary(weather$MaxTemp))
summaryStat2 = paste(names(sdata2),format(sdata2,digit=2),collapse = ", ")
boxplot(weather$MaxTemp,weather$MinTemp,
        main = "Minimum and Maximum Temperature in the US",
        at = c(1,2),
        names = c("Max","Min"),
        las = 2,
        col = c("orange","skyblue"),
        border = "black",
        horizontal = TRUE,
        xlab="Temperature (°C)")
title(sub = "Min Temperature Details (°C)", line = 4.5)
title(sub = summaryStat1, line = 5.5)
title(sub = "Max Temperature Details (°C)", line = 7.5)
title(sub = summaryStat2, line = 8.5)


#Analysis 1.2: Find the amount of sunshine and evaporation rates?
#Analysis 1.2 (Frequency Polygons - Relationship between Sunshine and Evaporation Rates)
colors <- c("Evaporation" = "skyblue", "Sunshine" = "orange")
ggplot(weather) + 
  geom_freqpoly(aes(x = Sunshine,color="Sunshine"), size = 1.5) +
  geom_freqpoly(aes(x = Evaporation,color="Evaporation"), size = 1.5) + 
  labs(title="Relationship between Sunshine and Evaporation Rates",x="Rates",y="Frequency",color="Legend") +
  scale_color_manual(values = colors) +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold",colour = "black"))


#Analysis 1.3: Find the number of reports of raining today?
#Analysis 1.3 (Bar Chart - Report of Raining Today)
ggplot(weather,aes(x=RainToday,fill = RainToday)) + 
  geom_bar(stat = "count") +
  geom_text(aes(label=format(round(((..count..)/length(weather$RainToday)*100),2))), 
            stat = "count",vjust = 1.5, colour = "black") +
  labs(title="Reports of Raining Today",x="Raining Today",y="Frequency") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold",colour = "black"))

#----------------------------------Question 2-----------------------------------
#Question 2: Is 9 am an excellent time to have a Formula One United States Grand Prix?

#Analysis 2.1: Find the wind direction and wind speed at 9 am?
#Analysis 2.1 (Compass Rose - Report of Wind Direction and Wind Speed at 9 am)
duplicate_weather1 = weather %>% filter(WindDir9am != "No Wind")
WindDegree9am=dir[as.character(duplicate_weather1$WindDir9am)]
plot.windrose(duplicate_weather1,spd=duplicate_weather1$WindSpeed9am,
              dir = WindDegree9am,
              title2 = "Wind Speed Based on Wind Direction at 9 am")

#Analysis 2.2: Find the number of reports of raining today?
#Analysis 2.2 (Bar Chart - Report of Raining Today)
ggplot(weather,aes(x=RainToday,fill = RainToday)) + 
  geom_bar(stat = "count") +
  geom_text(aes(label=format(round(((..count..)/length(weather$RainToday)*100),2))), 
            stat = "count", 
            vjust = 1.5, colour = "black") +
  labs(title="Reports of Raining Today",x="Raining Today",y="Frequency") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold",colour = "black"))


#Analysis 2.3: Find the sunshine exposure based on reports of raining today?
#Analysis 2.3 (Frequency Polygons Graph - Sunshine based on Report of Raining Today)
ggplot(weather,aes(x=Sunshine,color = RainToday)) + geom_freqpoly() +
  labs(title="Sunshine Based on Report of Raining Today",
       x="Sunshine",y="Frequency",color="Rain Today")+ 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,face="bold",
                                colour="black"),
        panel.background=element_rect(fill="linen"),
        plot.background=element_rect(fill="ivory3"))


#Analysis 2.4: Find the temperature at 9 am?
#Analysis 2.4 (Box Plot - Report of Temperature at 9am)
par(mar = c(7,4,4,2) + 0.1)
sdata3 = (summary(weather$Temp9am))
summaryStat3 = paste(names(sdata3),format(sdata3,digit=2),
                     collapse = "; ")
boxplot(weather$Temp9am,
        main ="Temperature at 9AM",
        xlab="Temperature (°C)",
        horizontal = TRUE,
        col = "skyblue")
title(sub = summaryStat3, line = 5.5)


#Analysis 2.5: Find the humidity at 9 am?
#Analysis 2.5 (Frequency Polygon - Report of Humidity at 9 am)
ggplot(weather, aes(x = Humidity9am, fill = Humidity9am)) + 
  geom_freqpoly() +  
  labs(title = "Humidity at 9AM",x="Humidity",y="Frequency") + 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,face="bold",colour="black"),
        panel.background=element_rect(fill="linen"),
        plot.background=element_rect(fill="ivory3"))

#----------------------------------Question 3-----------------------------------
#Question 3: Is 3 pm a good time to have a golf competition?

#Analysis 3.1: Find the wind direction and wind speed at 3 pm?
#Analysis 3.1 (Compass Rose - Report on Wind Direction, and Wind Speed at 9am)
duplicate_weather2 = weather %>% filter(WindDir3pm != "No Wind")
WindDegree3pm=dir[as.character(duplicate_weather2$WindDir3pm)]
plot.windrose(duplicate_weather2,spd=duplicate_weather2$WindSpeed3pm,dir = WindDegree3pm,
              title2 = "Wind Speed Based on Wind Direction at 3 pm")

#Analysis 3.2: Find the number of reports of raining today?
#Analysis 3.2 (Bar Chart - Report of Raining Today)
ggplot(weather,aes(x=RainToday,fill = RainToday)) + 
  geom_bar(stat = "count") +
  geom_text(aes(label=format(round(((..count..)/length(weather$RainToday)*100),2))), 
            stat = "count", vjust = 1.5, colour = "black") +
  labs(title="Reports of Raining Today",x="Raining Today",y="Frequency") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold",colour = "black"))

#Analysis 3.3: Find the temperature at 3 pm?
#Analysis 3.3 (Box Plot - Report on the temperature at 3 pm)
par(mar = c(7,4,4,2) + 0.1)
sdata4 = (summary(weather$Temp3pm))
summaryStat4 = paste(names(sdata4),format(sdata4,digit=2),collapse = "; ")
boxplot(weather$Temp3pm,main ="Temperature at 3PM",xlab="Temperature (°C)",
        horizontal = TRUE,col = "orange")
title(sub = summaryStat4, line = 5.5)

#Analysis 3.4: Find the humidity at 3 pm?
#Analysis 3.4 (Frequency Polygon Graph - Report of Humidity at 3pm)
ggplot(weather, aes(x = Humidity3pm, fill = Humidity3pm)) + 
  geom_freqpoly() +  
  labs(title = "Humidity at 3PM",x="Humidity",y="Frequency") + 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,face="bold",colour="black"),
        panel.background=element_rect(fill="linen"),
        plot.background=element_rect(fill="ivory3"))


#----------------------------------Question 4-----------------------------------
#Question 4: When is a good time for planes to depart and land in the United States airport?

#Analysis 4.1: Find the wind direction and wind speed at 9 am and 3 pm?
#Analysis 4.1 (Compass Rose - Report of Wind Direction and Wind Speed at 9am and 3pm)
duplicate_weather3 = weather %>% filter(WindDir9am != "No Wind")
WindDegree9am=dir[as.character(duplicate_weather3$WindDir9am)]
p1 = plot.windrose(duplicate_weather3,spd=duplicate_weather3$WindSpeed9am,dir = WindDegree9am,
                   title2 = "Wind Speed Based on Wind Direction at 9 am")

duplicate_weather4 = weather %>% filter(WindDir3pm != "No Wind")
WindDegree3pm=dir[as.character(duplicate_weather4$WindDir3pm)]
p2 = plot.windrose(duplicate_weather4,spd=duplicate_weather4$WindSpeed3pm,dir = WindDegree3pm,
                   title2 = "Wind Speed Based on Wind Direction at 3 pm")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

#Analysis 4.2: Find the humidity at 9 am and 3 pm?
#Analysis 4.2 (Frequency Polygon Graph - Report of Humidity at 9 am and 3 pm)
colors = c("9am" = "skyblue", "3pm" = "orange")
ggplot(weather) + 
  geom_freqpoly(aes(x = Humidity9am ,color="9am"),size=1.5) + 
  geom_freqpoly(aes(x = Humidity3pm , color= "3pm"),size=1.5) +
  labs(title = "Humidity",x="Humidity Rate",y="Frequency",color="Humidity") + 
  scale_color_manual(values = colors) +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,face="bold",colour="black"),
        panel.background=element_rect(fill="linen"),
        plot.background=element_rect(fill="ivory3")) 

#Analysis 4.3: Find the cloud formation at 9 am and 3 pm?
#Analysis 4.3 (Bar Chart - Cloud Formation at 9 am and 3 pm)
colors2 = c("9am" = "skyblue", "3pm" = "orange")
ggplot(weather) + 
  geom_bar(aes(x = Cloud9am ,color="9am"),size=1,alpha=0.1) + 
  geom_bar(aes(x = Cloud3pm , color= "3pm"),size=1,alpha=0.1) +
  labs(title = "Cloud Formation",x="Cloud Formation",y="Frequency",
       color="Cloud") + 
  scale_color_manual(values = colors2) +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,face="bold",colour="black"),
        panel.background=element_rect(fill="linen"),
        plot.background=element_rect(fill="ivory3")) 

#Analysis 4.4: Find the temperature at 9 am and 3 pm?
#Analysis 4.4 (Box plot - Temperature at 9 am and 3 pm)
par(mar = c(10,4,4,2) + 0.1)
sdata5 = (summary(weather$Temp9am))
summaryStat5 = paste(names(sdata5),format(sdata5,digit=2),collapse = ", ")
sdata6 = (summary(weather$Temp3pm))
summaryStat6 = paste(names(sdata6),format(sdata6,digit=2),collapse = ", ")
boxplot(weather$Temp3pm,weather$Temp9am,
        main = "Temperature in the US",
        at = c(1,2),
        names = c("3 PM","9 AM"),
        las = 2,
        col = c("orange","skyblue"),
        border = "black",
        horizontal = TRUE,
        xlab="Temperature (°C)")
title(sub = "9 AM Temperature Details (°C)", line = 4.5)
title(sub = summaryStat5, line = 5.5)
title(sub = "3 PM Temperature Details (°C)", line = 7.5)
title(sub = summaryStat6, line = 8.5)



#----------------------------------Question 5----------------------------------#
#Question 5: Is it possible to have an outdoor activity tomorrow?

#Analysis 5.1: Find the number of reports of raining tomorrow?
#Analysis 5.1 (Bar Chart - Reports of Rain Today)
ggplot(weather,aes(x=RainTomorrow,fill =RainTomorrow)) + 
  geom_bar(stat = 'count')  +
  geom_text(aes(label=format(round(((..count..)/length(weather$RainTomorrow)*100),2))), 
            stat = "count",vjust = 1.5, colour = "black") +
  labs(title="Reports of Raining Tomorrow",x="Raining Tomorrow",y="Frequency") +
  theme_bw() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold",colour = "black"))

#Analysis 5.2: Find how heavy is the rain tomorrow, if it rains? 
#Analysis 5.2 (Box Plot - Size of Rain Tomorrow)
duplicate_weather5 = weather %>% filter(RainTomorrow == "Yes")
par(mar = c(7,4,4,2) + 0.1)
sdata7 = (summary(duplicate_weather5$RISK_MM))
summaryStat7 = paste(names(sdata7),format(sdata7,digit=2),collapse = "; ")
boxplot(duplicate_weather5$RISK_MM,main = "Amount of Rain",col="skyblue",border = "black",
        horizontal = TRUE,xlab="Rain Size (mm)",ylab="Rain Tomorrow") 
title(sub = summaryStat7, line = 5.5)
