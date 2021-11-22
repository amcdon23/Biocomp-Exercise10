######McDonald Biocomputing Exercise 10######

#clear working environment
rm(list=ls())

####Question 1 - Data from my summer research project 

##Load in Dataset 
trees <- read.csv("McDonald.Tree.Data.csv")

##Need to log transform data for the allometries (linear relationships) to work 
lDBH <- log(trees$DBH)
lCA <- log(trees$`Crown.Area`)
lCV <- log(trees$`Crown.Volume`)
lD <- log(trees$`Crown.Depth`)
lCH <- log(trees$CH)
lCB <- log(trees$CB)

trees_log <- cbind(trees, lDBH, lCA, lCV, lD, lCH, lCB)
trees_log

##Subset data by species (5 total)
#sugar maple
ACSA <- subset(trees_log, trees_log$Species == "Acsa")
#quaking aspen
POTR <- subset(trees_log, trees_log$Species == "Potr")
#basswood
TIAM <- subset(trees_log, trees_log$Species == "Tiam")
#ash
FRNI <- subset(trees_log, trees_log$Species == "Frni")
#paper birch
BEPA <- subset(trees_log, trees_log$Species == "Bepa")

#Plot DBH vs. Crown Volume for each species 
  #set asthetics
  ggplot(trees_log, aes(x = lCV, y = lCA)) +
  #specify type of plot
  geom_point() +
  #title x and y axis
  xlab("Log(DBH)") +
  ylab("Log(Crown Volume)") + 
  #add regression and confidence interval around 
  geom_smooth(method = "lm") +
  #wrap for each species
  facet_wrap(vars(Species))
  
  
####Question 2 - bar and scatter for data.txt
  
##read in data
region <- read.csv("data.txt")

###BAR PLOT 
#create plot with region on x and observations on y
ggplot(region, aes(x=region,y=observations))+
  #specify bar plot and mean of oobservations as y axis
  stat_summary(fun=mean, geom="bar")+
  #change x-axis title
  xlab("Region")+
  #change y-axis title
  ylab("Mean Population")+
  #change theme
  theme_classic()

###SCATTERPLOT
ggplot(data=region, aes(x = region, y=observations))+
  #specify scatterplot
  geom_jitter(aes(alpha=0.1))+
  #specify x-axis title 
  xlab("Region")+
  #specify y-axis title
  ylab("Number of observations")+
  #specify classic theme
  theme_classic()

###Do the bar and scatter plots tell you different stories? Why?
#ANSWER: Yes these two plots tell very different stories because while the bar
#chart makes it seem as though the observations for each region are all very 
#similar as their means are very similar, when looking at the scatterplot, it 
#is clear that each of the regions has extremely different distributions of the 
#data even though the means are very similar. In this case, thee bar graph is 
#very misleading and the scatter plot is needed to get an accurate representation
#of the data. 