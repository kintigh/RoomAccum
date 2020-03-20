#One time: Be sure to install packages
#          Tools>Install Packages then add ggplot2,plyr,gmodels,deducer
#
install.packages("tidyverse")
#install.packages("psych")
#install.packages("ca")
#install.packages("Deducer")

#Each Time:In Packages tab at lower right, check plyr, ggplot2, Psych, labdsv, ca

#library("ca")
#library("MASS")
library("ggplot2")
library("plyr")

#
getwd()
setwd("c:/Users/kintigh/Dropbox (ASU)/Cibola/papers/RoomAccum")

ds <- read.csv("roomhesh6o.csv")
ds

initrmlst <-c(8,12,16) #depends on initrm
plotuse <- c(10,15,20,25)

summary(ds)
by(ds,ds$UseLife,summary)

quantile(per1$Rate,ps)
quantile(per1$PerMidRm,ps)

#need to limit by SD=0
png("Fig0-box.png")
boxplot(Rate~Period,varwidth=TRUE,horizontal=TRUE,xlab="Rate (%)")
dev.off()
boxplot(PerMidRm~Period,varwidth=TRUE,horizontal=TRUE,xlab="Rate (%)")
plot(Rate,Period) #dot plot 


#=========================================
#explore SD for given uselife & init rooms
#=========================================
#need to rerun to get more SDs

plotinitrm<-13
plotuse <- 25
plotsd=0

png("Fig1-SDa.png",width=6.5,height=4,units="in",res=256)
label<-paste("Sensitivity to SD of Room Uselife (0=blue 5=gr 10=re 15=or 20=pu)"," UseLife=",plotuse," InitRooms=",plotinitrm,"",sep="")

p1 <- ggplot()
p1 + 
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==0) & (UseLife==plotuse)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==5) & (UseLife==plotuse)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==10) & (UseLife==plotuse)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==15) & (UseLife==plotuse)), aes(x=Period, y=Rate), color="orange") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==20) & (UseLife==plotuse)), aes(x=Period, y=Rate), color="purple") +
     theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
dev.off()

#====================================
# Explore Uselife with optimum InitRm 
#====================================

plotsd <- 0

label<-paste("Sensitivity to Room UseLife (15=red 20=gr 25=bl 30=pu) "," RoomSD=",plotsd,"",sep="")
png("Fig2-Uselife.png",width=6.5,height=4,units="in",res=256)
p1 <- ggplot()
p1 + 
  geom_line(data=subset(ds,(InitRm==9) & (UseSD==plotsd) & (UseLife==15)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==11) & (UseSD==plotsd) & (UseLife==20)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==13) & (UseSD==plotsd) & (UseLife==25)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==16) & (UseSD==plotsd) & (UseLife==30)), aes(x=Period, y=Rate), color="purple") +
  theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
dev.off()

#===============================
# Plot 2 smooth lines from above for Use=20,25
#===============================
plotsd <- 0

label<-paste("Sensitivity to Room UseLife (gr: Use=20,init=11; bl:use=25,init=13) "," RoomSD=",plotsd,"",sep="")

png("Fig2a-Uselife2.png",width=6.5,height=4,units="in",res=256)
p1 <- ggplot()
p1 + 
#  geom_line(data=subset(ds,(InitRm==9) & (UseSD==plotsd) & (UseLife==15)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==11) & (UseSD==plotsd) & (UseLife==20)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==13) & (UseSD==plotsd) & (UseLife==25)), aes(x=Period, y=Rate), color="blue") +
#  geom_line(data=subset(ds,(InitRm==16) & (UseSD==plotsd) & (UseLife==30)), aes(x=Period, y=Rate), color="purple") +
  theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
dev.off()

#===============================
# Plot Modeled vs Compount Interest Rates
#===============================
dsrate <- read.csv("heshrates.csv")
use<-25
initrm<-13

label<-paste("Growth Rates: Modeled=Blue, Compound Interest=Red",sep="")

png("Fig5h-rates.png",width=6.5,height=4,units="in",res=256)
p2 <- ggplot()
p2 + 
  geom_line(data=subset(dsrate,(Approach==1)), aes(x=Year, y=Rate), color="blue") +
  geom_line(data=subset(dsrate,(Approach==2)), aes(x=Year, y=Rate), color="red") +
  geom_line(data=subset(dsrate,(Approach==0)), aes(x=Year, y=Rate), color="black") +
  theme_bw() + 
  scale_x_continuous(name='Year', breaks=seq(900,1275,25)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
dev.off()


#===============================
# Plot Modeled vs Compount Interest Rates
#===============================
dsrate <- read.csv("obaprates.csv")
use<-25
initrm<-13

label<-paste("Growth Rates: Modeled=Blue, Compound Interest=Red",sep="")

png("Fig5o-rates.png",width=6.5,height=4,units="in",res=256)
p2 <- ggplot()
p2 + 
  geom_line(data=subset(dsrate,(Approach==1)), aes(x=Year, y=Rate), color="blue") +
  geom_line(data=subset(dsrate,(Approach==2)), aes(x=Year, y=Rate), color="red") +
  geom_line(data=subset(dsrate,(Approach==0)), aes(x=Year, y=Rate), color="black") +
  theme_bw() + 
  scale_x_continuous(name='Year', breaks=seq(900,1275,25)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
dev.off()

#===============================
# Plot initrm +/- 1 either side of optimum
#===============================
use<-25
init<-13
label<-paste("Sensitivity to InitRm - UseLife=",use," InitRm=",init," (bl=0 red=-1 gr=+1) "," RoomSD=",plotsd,"",sep="")

png("Fig3a-Init25.png",width=6.5,height=4,units="in",res=256)
p1 <- ggplot()
p1 + 
  geom_line(data=subset(ds,(InitRm==init) & (UseSD==plotsd) & (UseLife==use)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==(init-1)) & (UseSD==plotsd) & (UseLife==use)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==(init+1)) & (UseSD==plotsd) & (UseLife==use)), aes(x=Period, y=Rate), color="green") +
  theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
dev.off()

use<-20
init<-11
label<-paste("Sensitivity to InitRm - UseLife=",use," InitRm=",init," (bl=0 red=-1 gr=+1) "," RoomSD=",plotsd,"",sep="")

png("Fig3a-Init20.png",width=6.5,height=4,units="in",res=256)
p1 <- ggplot()
p1 + 
  geom_line(data=subset(ds,(InitRm==init) & (UseSD==plotsd) & (UseLife==use)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==(init-1)) & (UseSD==plotsd) & (UseLife==use)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==(init+1)) & (UseSD==plotsd) & (UseLife==use)), aes(x=Period, y=Rate), color="green") +
  theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
dev.off()

#=============================
# Plot Population Hesh
#=============================

dspop <- read.csv("roomhesh6_y.csv")
dsimp <- read.csv("implieddsh.csv")

plotuse<-25
plotinitrm <- 13
plotsd <- 0

label<-paste("Hesh Occupied Rooms","  Modeled=blue; Implied=red",sep="")

png("Fig4h-Pop.png",width=6.5,height=4,units="in",res=256)
p1 <- ggplot()
p1 + 
  geom_line(data=subset(dspop,(InitRm==plotinitrm)), aes(x=Year, y=OccRm), color="blue") +
  geom_line(data=dsimp,aes(x=Year, y=OccRm), color="red") +
  theme_bw() + 
  scale_x_continuous(name='Year', breaks=seq(900,1275,25)) + 
  scale_y_continuous(name='Occupied Rooms', limits=c(0,250), breaks=seq(0,250,25)) +
  ggtitle(label)
dev.off()


#=============================
# Plot Population OBAP
#=============================

dspop <- read.csv("roomobap_y.csv")
dsimp <- read.csv("implieddso.csv")

plotinitrm <- 28
plotsd <- 0

label<-paste("OBAP Occupied Rooms","  Modeled=blue; Implied=red",sep="")

png("Fig4o-Pop.png",width=6.5,height=4,units="in",res=256)
p1 <- ggplot()
p1 + 
  geom_line(data=subset(dspop,(InitRm==plotinitrm)), aes(x=Year, y=OccRm), color="blue") +
  geom_line(data=dsimp,aes(x=Year, y=OccRm), color="red") +
  theme_bw() + 
  scale_x_continuous(name='Year', breaks=seq(1000,1275,25)) + 
  scale_y_continuous(name='Occupied Rooms', limits=c(0,400), breaks=seq(0,400,25)) +
  ggtitle(label)
dev.off()



#=======================================
#Explore initrm for fixed uselife and sd  special case of above
#=======================================
p1 <- ggplot()
ggplot() +
  # geom_line(data=subset(ds,(InitRm==5) & (UseSD==0) & (UseLife==25)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==14) & (UseSD==0) & (UseLife==25)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==13) & (UseSD==0) & (UseLife==25)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==12) & (UseSD==0) & (UseLife==25)), aes(x=Period, y=Rate), color="purple") +
  theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle('Sensitivity to InitRm G=8,B=12,P=16 (Uselife=25, RoomSD=0)') 




#===========================================
# Explore uselife in for loop - doesn't work
#===========================================

plotuse<-25
initrmlst<-c(12,13,14)
plotsd<-0

for (plotinitrm in initrmlst) {
#  for (plotsd in usesdlst) {
  print(paste(plotinitrm,plotsd))

label<-paste("Sensitivity to Room UseLife (10=red 15=gr 20=bl 25=pu) InitRooms=",plotinitrm," RoomSD=",plotsd,"",sep="")
label

p1 <- ggplot()

p1 + 
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==10)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==15)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==20)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==25)), aes(x=Period, y=Rate), color="purple") +
  theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label)
  print(p1)
#  } 
}
#============
#Doesn't WOrk
#============

plotuse<-25
initrmlst<-c(12,13,14)
Label<-"Test"
p1 <- ggplot()
p1 +
  geom_line(data=subset(ds,(UseLife==plotuse) & (InitRm %in% initrmlst) & (UseSD==plotsd)), aes(x=Period, y=Rate, color=InitRm)) +
  theme_bw() +
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle(label) 

dstest<-subset(ds,(UseLife==plotuse) & (InitRm %in% initrmlst) & (UseSD==plotsd))
dstest

#============================
#Explore initroom and uselife
=============================
#lines with orange dots sd 0 and init 8; lines with red dotes sd 5 and init 12; lines with purple dots sd 10 and init 16
plotinitrm <- 8
plotrminc <- 4
plotsd <- 0
plotsdinc <-5

p1 <- ggplot()

p1 + 
# geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==10)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==15)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==20)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==25)), aes(x=Period, y=Rate), color="purple") +

# geom_point(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==10)), aes(x=Period, y=Rate), color="orange") +
  geom_point(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==15)), aes(x=Period, y=Rate), color="orange") +
  geom_point(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==20)), aes(x=Period, y=Rate), color="orange") +
  geom_point(data=subset(ds,(InitRm==plotinitrm) & (UseSD==plotsd) & (UseLife==25)), aes(x=Period, y=Rate), color="orange") +
  
# geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==10)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==15)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==20)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==25)), aes(x=Period, y=Rate), color="purple") +
 
# geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==10)), aes(x=Period, y=Rate), color="red") +
  geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==15)), aes(x=Period, y=Rate), color="red") +
  geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==20)), aes(x=Period, y=Rate), color="red") +
  geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc)) & (UseSD==(plotsd+plotsdinc)) & (UseLife==25)), aes(x=Period, y=Rate), color="red") +
  
# geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==10)), aes(x=Period, y=Rate), color="red") +
  geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==15)), aes(x=Period, y=Rate), color="green") +
  geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==20)), aes(x=Period, y=Rate), color="blue") +
  geom_line(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==25)), aes(x=Period, y=Rate), color="purple") +
  
# geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==10)), aes(x=Period, y=Rate), color="purple") +
  geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==15)), aes(x=Period, y=Rate), color="purple") +
  geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==20)), aes(x=Period, y=Rate), color="purple") +
  geom_point(data=subset(ds,(InitRm==(plotinitrm+plotrminc*2)) & (UseSD==(plotsd+plotsdinc*2)) & (UseLife==25)), aes(x=Period, y=Rate), color="purple") +
  
  theme_bw() + 
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  ggtitle('Sensitivity to UseLife R=10,G=15,B=20,P=25 (InitRooms=var, RoomSD=0)') 


#====================
# Legend Doesn't work
#====================

cols <- c("UL10"="red","UL15"="green", "UL20"="blue","UL25"="purple")
brk <- c("UL10","UL15","UL20","UL25")
cols

p = ggplot() +
 # theme_bw() +
 #  theme(legend.position = c(0,0)) +
  scale_colour_manual("Legend", values = cols, breaks = brk) #+

  theme(legend.position = "left") +
  scale_color_manual("Legend", values = cols, breaks = brk)
print(p)

# save plots as .png
# ggsave(plot, file=paste(results, 
#                        'projection_graphs/county_graphs/',
#                        county_list[i], ".png", sep=''), scale=2)

# save plots as .pdf
# ggsave(plot, file=paste(results, 
#                        'projection_graphs/county_graphs/',
#                        county_list[i], ".pdf", sep=''), scale=2)


mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})

p1 <- ggplot(mtcars2) +
  geom_point(aes(x = wt, y = mpg, colour = gear)) +
  labs(title = "Fuel economy declines as weight increases",
       subtitle = "(1973-74)",
       caption = "Data from the 1974 Motor Trend US magazine.",
       tag = "Figure 1",
       x = "Weight (1000 lbs)",
       y = "Fuel economy (mpg)",
       colour = "Gears")

p1 + theme_gray() # the default
p1 + theme_bw()
p1 + theme_linedraw()
p1 + theme_light()
p1 + theme_dark()
p1 + theme_minimal()
p1 + theme_classic()
p1 + theme_void()

# Theme examples with panels

p2 <- p1 + facet_grid(vs ~ am)

p2 + theme_gray() # the default
p2 + theme_bw()
p2 + theme_linedraw()
p2 + theme_light()
p2 + theme_dark()
p2 + theme_minimal()
p2 + theme_classic()
p2 + theme_void()
