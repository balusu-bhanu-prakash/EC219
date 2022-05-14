#EC219 project: Analysis of Formula 1 racing by group 2.

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggrepel)
library(viridis)
library(circlize)
#importing required libraries for virtualization.

results<-read.csv('~/@IIITD/Semester 4/EC219/Mini project/results.csv',sep=',',stringsAsFactors=F)
#reading the data

results$fastestLapSpeed<-as.numeric(results$fastestLapSpeed)
#convert string to numerical

convertFastestLap<-function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}
#A custom function to convert time from minutes(mm:SS) to seconds

results$fastestLapTimeNum<-sapply(results$fastestLapTime, convertFastestLap)
#Converting fastest lap times into seconds. 

races<-read.csv('~/@IIITD/Semester 4/EC219/Mini project/races.csv',stringsAsFactors=F,sep=',')
#reading the data

races$date<-as.Date(races$date,"%Y-%m-%d")
#converting string to Date

races$name<-gsub(" Grand Prix","",races$name)
#remove "Grand Prix" in the name for easy use.
results_2<-left_join(
  results %>% dplyr::select(-time, -fastestLapTime), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')

circuits<-read.csv("~/@IIITD/Semester 4/EC219/Mini project/circuits.csv",sep=",",stringsAsFactors=F)
races<-left_join(races %>% select(-name,-url), circuits %>% select(-url), by='circuitId')
#further grou[ing the items in all datafiles.
results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name,year) %>% 
  summarize(medianFastestLapSpeed = median(fastestLapSpeed,na.rm=T)) %>% 
  ggplot(aes(x=factor(year),y= medianFastestLapSpeed,color=medianFastestLapSpeed)) + 
  geom_point() + theme_fivethirtyeight() + 
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) +
  theme(
    axis.text.x = element_text(size=6,angle=45),
    strip.text.x = element_text(size = 10)) + facet_wrap(~name,ncol=9) + 
  labs(title='Fastest Lap per Circuit, from 2005 to 2017',
       subtitle='speed in km/h') +
  guides(color=FALSE)

results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name,year) %>% 
  summarize(medianFastestLapSpeed = median(fastestLapSpeed,na.rm=T)) %>% 
  ggplot(aes(x=factor(year),y= medianFastestLapSpeed,color=medianFastestLapSpeed)) + 
  geom_boxplot(alpha=.25) + theme_fivethirtyeight() + 
  geom_jitter(shape=16,position=position_jitter(0.2),size=1.5) + 
  geom_smooth(method='loess',aes(group=1),color='red',lty=2,size=.5) +
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) + 
  labs(title='Fastest Lap per Year',
       subtitle='in km/h, grouped by Grand Prix') + 
  guides(color = FALSE)

results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name) %>% 
  ggplot(aes(x=fastestLapSpeed)) + 
  geom_histogram(bins=100) + theme_fivethirtyeight() + 
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) +
  theme(
    axis.text.x = element_text(size=6,angle=45),
    strip.text.x = element_text(size = 10)) + facet_wrap(~name,ncol=9) + 
  labs(title='Fastest Lap distribution per Circuit',
       subtitle='speed in km/h, grouped by years') +
  guides(color=FALSE)

results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name,year) %>% 
  summarize(medianFastestLapTimeNum = median(fastestLapTimeNum,na.rm=T)) %>% 
  ggplot(aes(x=factor(year),y= medianFastestLapTimeNum, color=medianFastestLapTimeNum)) +
  geom_boxplot(alpha=.25) + theme_fivethirtyeight() + 
  geom_jitter(shape=16,position=position_jitter(0.2),size=1.5) + 
  geom_smooth(method='loess',aes(group=1),color='red',lty=2,size=.5) +
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) + 
  labs(title='Lap time per Year',
       subtitle='in seconds, grouped by Grand Prix') + 
  guides(color = FALSE)

results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name,year) %>% 
  summarize(medianFastestLapTimeNum = median(fastestLapTimeNum,na.rm=T)) %>% 
  ggplot(aes(x=factor(year),y= medianFastestLapTimeNum, color=medianFastestLapTimeNum)) +
  geom_point() + theme_fivethirtyeight() + 
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) +
  theme(
    axis.text.x = element_text(size=6,angle=45),
    strip.text.x = element_text(size = 10)) + facet_wrap(~name,ncol=9) + 
  labs(title='Lap time per Year, from 2005 to 2017',
       subtitle='in seconds') +
  guides(color=FALSE)


drivers<-read.csv('~/@IIITD/Semester 4/EC219/Mini project/drivers.csv',sep=',',stringsAsFactors=F)
#calculate the driver's age in 2017 as data is available only until 2017.
drivers$age_driver <- 2017 - sapply(drivers$dob, function(x) as.numeric(strsplit(x,'/')[[1]][3]))
#load drivers Standings
driversStandings<-read.csv('~/@IIITD/Semester 4/EC219/Mini project/driverStandings.csv',sep=',',stringsAsFactors=F)
drivers<-left_join(drivers %>% select(-url), driversStandings,by='driverId')


results_3<-left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 

results_3<-left_join(results_3,races %>% select(-time), by='raceId')

winsDis<-results_3 %>% 
  filter(position==1) %>% 
  group_by(driverRef, circuitRef) %>% 
  summarize(count=n()) %>%
  mutate(allWins = sum(count)) %>%
  ggplot(aes(x=allWins)) +
  geom_histogram(bins=50) + theme_fivethirtyeight() + ggtitle("Distribution of the number of victories")

winsBar<-results_3 %>% 
  dplyr::filter(position==1) %>% 
  dplyr::group_by(driverRef, circuitRef) %>% 
  dplyr::summarize(count=n()) %>%
  dplyr::mutate(allWins = sum(count)) %>% 
  dplyr::filter(allWins>2) %>%
  ggplot(aes(x=reorder(driverRef, allWins),y= count)) +
  geom_bar(aes(fill=circuitRef),stat='identity',color='white',size=.1) + 
  coord_flip() + theme_fivethirtyeight() + 
  scale_fill_manual(name="",values = viridis::viridis(71)) +
  guides(fill=guide_legend(ncol=5)) + 
  theme(legend.text= element_text(size=10),
        legend.key.size = unit(.1, "cm"),
        legend.position=c(.65,.20)) + 
  labs(title="Number of victories per Driver",
       subtitle="only drivers with 2 or more wins are shown.")
winsBar + annotation_custom(grob = ggplotGrob(winsDis), xmin = 22, xmax = 50, ymin = 31, ymax = 90)
#plotted plots based on driver's data now moving onto constructors data.
constructors<-read.csv('~/@IIITD/Semester 4/EC219/Mini project/constructors.csv',sep=',',stringsAsFactors=F)
constructorStandings<-read.csv('~/@IIITD/Semester 4/EC219/Mini project/constructorStandings.csv',sep=',',stringsAsFactors=F)
constructorResults<-read.csv("~/@IIITD/Semester 4/EC219/Mini project/constructorResults.csv",sep=",",stringsAsFactors=F)

constructorResults<-left_join(
  constructorResults, 
  races %>% rename(name_races = name), by='raceId')

constructorResults <- left_join(constructorResults, constructors %>% select(-url) %>% rename(name_constructor = name), by='constructorId')

constructorResults <- left_join(constructorResults, constructorStandings %>% rename(point_constructor = points) %>% select(-X), by=c('constructorId','raceId'))

winConstructors<-constructorResults %>% 
  filter(wins == 1) %>% 
  group_by(name_constructor) %>% 
  summarize(count=n()) %>% 
  filter(count>0) %>%
  ggplot(aes(x=reorder(name_constructor, count),y= count,fill=count)) +
  geom_bar(stat='identity',color='white',size=.1) + 
  coord_flip() + theme_fivethirtyeight() + 
  scale_fill_gradientn(name="",colors = viridis::viridis(10)) +
  guides(fill=guide_legend(ncol=3)) + 
  theme(legend.text= element_text(size=10),
        legend.key.size = unit(.1, "cm"),
        legend.position=c(.65,.20)) + 
  labs(title="Number of victories per Constructor",
       subtitle="only Constructor with 1 or more wins are shown.") + guides(fill=F)

top5Constructors<-constructorResults %>% 
  filter(name_constructor %in% c('Ferrari','McLaren','Williams','Brabham','BRM')) %>% 
  filter(wins == 1) %>% group_by(name_constructor,year) %>%
  summarize(count=n()) %>% 
  ggplot(aes(x=factor(year),y=count)) +
  geom_histogram(aes(fill=name_constructor),
                 stat='identity',
                 position="fill",
                 size=1.5) + 
  theme_fivethirtyeight() + scale_fill_brewer(name="",palette='Paired') +
  theme(axis.text.x = element_text(size=8,angle=45)) + ggtitle("Top 5 constructors's wins per year")
winConstructors + 
  annotation_custom(grob = ggplotGrob(top5Constructors), xmin = 20, xmax = 0, ymin = 20, ymax = 200)


#Now analysis for famous japanase GP 1988/89.
y1988 <- results_3 %>% 
  dplyr::filter(driverRef=='prost' | driverRef=='senna')%>% 
  filter(year==1988) %>% 
  select(date,driverRef,points) %>% mutate(winRace = ifelse(points==9,'yes','no')) %>%
  group_by(driverRef) %>% 
  mutate(current = cumsum(points))  %>% 
  ggplot(aes(x=date,y=current,color=driverRef)) + 
  geom_line(size=2,alpha=.5) + geom_point(aes(shape=winRace),color='black',size=2) + 
  theme_fivethirtyeight() + scale_color_brewer(name="",palette='Set1') + ylim(0,120) + 
  labs(title="Points accumulated during Season 1988",subtitle = 'triangle indicates a race win, circle otherwise') + 
  theme(legend.position='right',legend.direction='vertical') + guides(shape=FALSE)

y1989<-results_3 %>% 
  dplyr::filter(driverRef=='prost' | driverRef=='senna') %>%  
  filter(year==1989) %>% 
  select(date,driverRef,points) %>% mutate(winRace = ifelse(points==9,'yes','no')) %>%
  group_by(driverRef) %>% 
  mutate(current = cumsum(points)) %>% 
  ggplot(aes(x=date,y=current,color=driverRef)) + 
  geom_line(size=2,alpha=.5) + geom_point(aes(shape=winRace),color='black',size=2) + 
  theme_fivethirtyeight() + scale_color_brewer(name="",palette='Set1') + 
  labs(title="Points accumulated during Season 1989",subtitle = 'triangle indicates a race win, circle otherwise\n reference : https://www.motorsport.com/f1/news/25-years-ago-today-a-rivalry-became-legendary-1989-japanese-gp/') + 
  theme(legend.position='right',legend.direction='vertical') + guides(shape=FALSE) + 
  annotate("text", x=as.Date("1989-10-15"), y = 68, label = "The Japanese incident[1]", size=3, colour="black") + 
  geom_curve(aes(x = as.Date("1989-10-15"), y = 71, xend = as.Date("1989-10-22"), yend = 79), curvature = .05,arrow = arrow(length = unit(0.02, "npc")),color='black')

grid.arrange(y1988, y1989, ncol=1)

#Now analysis for the legendary driver Michael Schumacher.
results_4<-
  left_join(
    results %>% select(-time) %>% rename(number_results = number), 
    drivers %>% select(-points,-position,-positionText) %>% rename(number_drivers = number, driver_nationality = nationality),by=c('driverId','raceId')) %>% 
  left_join(constructorResults %>% select(-points, -position, -positionText, -X) %>% rename(wins_constructor = wins, nationality_constructor = nationality), by=c('raceId','constructorId'))
g1<-results_4 %>% filter(driverRef == 'michael_schumacher') %>% 
  group_by(name_constructor, year) %>% summarize(countByConstructor = n()) %>% 
  ggplot(aes(x=factor(year),y=countByConstructor,fill=name_constructor)) + 
  geom_histogram(stat='identity') + theme_fivethirtyeight() + 
  scale_fill_manual(name='',values =c(Benetton='#87CEEB',Ferrari='#EE0000',Jordan='#FFD700',Mercedes='#7F7F7F')) + 
  theme(legend.position='top',
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank()) + 
  labs(title='The Red baron case : Michael Schumacher',subtitle='top : by constructor, bottom : cumulated')

g2<-results_4 %>% filter(driverRef == 'michael_schumacher') %>% 
  group_by(year) %>% summarize(count=n()) %>% mutate(cumulRaces = cumsum(count)) %>% 
  ggplot(aes(x=factor(year),y=cumulRaces))+ geom_histogram(stat='identity') + 
  theme_fivethirtyeight() + theme(legend.position='None')
grid.arrange(g1, g2, ncol=1, nrow=2, heights=c(4, 1))


#finding the top 10 drivers, by their number of wins
temp<-(results_3 %>% filter(position==1) %>% group_by(driverRef) %>% summarize(count=n()) %>% arrange(-count) %>% top_n(10))$driverRef
results_3$top10<-ifelse(results_3$driverRef %in% temp,results_3$driverRef,'other')
results_3 %>% filter(position==1) %>% group_by(top10,year) %>% 
  summarize(count=n()) %>% filter(count>0 & year>=1960) %>% 
  ggplot(aes(x=factor(year),y=count)) + 
  geom_histogram(aes(fill=top10),position="fill",stat='identity') + 
  theme_fivethirtyeight() + 
  theme(legend.position='bottom',
        axis.text.x = element_text(size=8,angle=45)) + 
  scale_fill_brewer(name="",palette='Paired') + guides(fill=guide_legend(ncol=13)) + 
  labs(title='Proportion of wins per Driver and year',
       subtitle='only the top 10 drivers by number of wins are shown')

results_4<-left_join(
  results_3, 
  constructorResults %>% select(-position,-positionText,-points,-X,-country,-wins,-lng,-lat,-alt,-nationality,-circuitRef,-round, -circuitId,-year,-time,-date,-location),
  by=c('raceId','constructorId'))
temp<-data.frame(
  results_4 %>% filter(position==1) %>% 
    group_by(name_constructor, driverRef) %>% 
    summarize(count=n()) %>% filter(count>5) %>% na.omit())

#prepare colors for wheelchart.
names<-sort(unique(temp$name_constructor))
color <- c('#87CEEB',"gray50","gray50","#FFFFE0","gray50","#006400",'#EE0000','#1E90FF','gray50','#006400','#7F7F7F','#7F7F7F','#9C661F','#FFD700','gray50','gray50','#EEEEE0')
COL<-data.frame(name_constructor = names,color)
temp2<-data.frame(left_join(temp, COL, by='name_constructor'))

chordDiagram(temp2[,c(1:2)],transparency = 0.5, grid.col = append(color,rep('aliceblue',32)), col= as.character(temp2$color),annotationTrack = "grid", preAllocateTracks = 1)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(
      mean(xlim), 
      ylim[1], 
      sector.name, 
      facing = "clockwise", 
      niceFacing = TRUE, 
      adj = c(0, 0.25), 
      cex=.7)
    circos.axis(
      h = "top", 
      labels.cex = 0.5, 
      major.tick.percentage = 0.2, 
      sector.index = sector.name, 
      track.index = 2)
  }, 
  bg.border = NA)
