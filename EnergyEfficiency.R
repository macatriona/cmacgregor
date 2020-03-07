#Catriona MacGregor

#X1 Relative Compactness
#X2 Surface Area
#X3 Wall Area
#X4 Roof Area
#X5 Overall Height
#X6 Orientation (2-5) (N,S,E,W)
#X7 Glazing Area (0,0.1,0.25,0.4)
#X8 Glazing Area Distribution
#y1 Heating Load
#y2 Cooling Load

#install.packages("ggplot2")
library(ggplot2)
library(corrplot) #to see the correlations
library(dplyr)

d <- read.table("EnergyEff.txt", header=TRUE)
names(d) 
d<-rename(d,c("Relative.Compactness"="X1"))
d<-rename(d,c("Surface.Area"="X2"))
d<-rename(d,c("Wall.Area"="X3"))
d<-rename(d,c("Roof.Area"="X4"))
d<-rename(d,c("Overall.Height"="X5"))
d<-rename(d,c("Orientation"="X6"))
d<-rename(d,c("Glazing.Area"="X7"))
d<-rename(d,c("Glazing.Area.Distribution"="X8"))
d<-rename(d,c("Heating.Load"="Y1"))
d<-rename(d,c("Cooling.Load"="Y2"))

#categorical variables - renaming so that it's easier to understand
d$Orientation<-as.factor(d$Orientation)
levels(d$Orientation)<-c("North","East","South","West")

d$Glazing.Area <- as.factor(d$Glazing.Area)
levels(d$Glazing.Area) <- c("0%","10%","25%","40%")

d$Glazing.Area.Distribution <- as.factor(d$Glazing.Area.Distribution)
levels(d$Glazing.Area.Distribution) <- c("unknown","uniform","North","East","South","West")

#now with the fixed categorical variables we can better understand the summary
summary(d)

#let's look at the overall correlation without the categorical variables
corrplot(cor(d[,c(1,2,3,4,5,9,10)]))

#ordered bar chart for heating load
Heating.Avg.O <- aggregate(d$Heating.Load, by=list(d$Orientation),FUN=mean)
colnames(Heating.Avg.O)<- c("Orientation","Heating") #fixes the column names
Heating.Avg.O <- Heating.Avg.O[order(Heating.Avg.O$Heating),] #this is sorting the data
#keeping the order when we plot
Heating.Avg.O$Orientation <-factor(Heating.Avg.O$Orientation,levels=Heating.Avg.O$Orientation)  
head(Heating.Avg.O,4) #outputs the mean of the heating load for each orientation
ggplot(Heating.Avg.O,aes(x=Orientation,y=Heating))+geom_bar(stat="identity", width = 0.6, fill="coral1")+
labs(title="Ordered Bar Chart",subtitle = "Orientation Vs Heating Load")

#ordered bar chart for cooling load
Cooling.O <- aggregate(d$Cooling.Load, by=list(d$Orientation),FUN=mean)
colnames(Cooling.O)<- c("Orientation","Cooling")
Cooling.O <- Cooling.O[order(Cooling.O$Cooling),]
Cooling.O$Orientation <-factor(Cooling.O$Orientation,levels=Cooling.O$Orientation)  
head(Cooling.O,4)
ggplot(Cooling.O,aes(x=Orientation,y=Cooling))+geom_bar(stat="identity", width = 0.6, fill="cornflowerblue")+ 
  labs(title="Ordered Bar Chart",subtitle = "Orientation Vs Cooling Load")



#Ordered bar chart for heating & glazing area
Heating.Avg.G <- aggregate(d$Heating.Load, by=list(d$Glazing.Area),FUN=mean)
colnames(Heating.Avg.G)<- c("Glazing","Heating")
Heating.Avg.G <- Heating.Avg.G[order(Heating.Avg.G$Heating),]
Heating.Avg.G$Glazing <-factor(Heating.Avg.G$Glazing,levels=Heating.Avg.G$Glazing)  
head(Heating.Avg.G,4)
ggplot(Heating.Avg.G,aes(x=Glazing,y=Heating))+geom_bar(stat="identity", width = 0.6, fill="coral1")+
  labs(title="Ordered Bar Chart",subtitle = "Glazing Area Vs Heating Load")

#ordered bar chart for glazing area
Cooling.G <- aggregate(d$Cooling.Load, by=list(d$Glazing.Area),FUN=mean)
colnames(Cooling.G)<- c("Glazing","Cooling")
Cooling.G <- Cooling.G[order(Cooling.G$Cooling),]
Cooling.G$Glazing<-factor(Cooling.G$Glazing,levels=Cooling.G$Glazing)  
head(Cooling.G,4)
ggplot(Cooling.G,aes(x=Glazing,y=Cooling))+geom_bar(stat="identity", width = 0.6, fill="cornflowerblue")+
  labs(title="Ordered Bar Chart",subtitle = "Glazing Vs Heating Load")

#roof area analysis with linear fit
ggplot(d, aes(x=Roof.Area, y=Heating.Load, color=Cooling.Load)) + geom_point() +
  geom_smooth(method=lm,se=TRUE)

