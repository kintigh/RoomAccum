#########################
#########################
## initialize libraries
library(dplyr)
library(ggplot2)
#########################
#########################

## Imporort data from RoomAccum12.exe program and create figures


# Figure 1 Data, Growth Rates by period for simulated and and compound interest methods of calculation 
# Variables as follows: input: RoomHesh6.txt, UseLife = 25, S.D. of UseLife = 0.0, InitRooms = 13
ds.Fig1 <- read.csv('Fig1_HeshRates.csv') 

# Plot Figure 1: Estimated growth rates for the Heshotauthla survey derived from the formula and the simulation 
png("Fig1.png",width=6.5,height=4,units="in",res=256)
  ds.Fig1 %>%
    ggplot(aes(x=Year, y=Rate, group=Approach, color=Approach)) +
    geom_line(size=0.75) +
    scale_x_continuous(name="Year", breaks=seq(900,1275,25)) + 
    scale_y_continuous(name="Growth Rate (%)", breaks=seq(-6,7,1)) +
    theme_bw() +
    theme(legend.position = c(0.01,0.99), 
            legend.justification = c(0, 1), 
            legend.text=element_text(size=6), 
            legend.title=element_text(size=8)) +
    geom_hline(yintercept = 0) +
    scale_color_discrete(name = "Growth Rates")
dev.off()  

#########################
#########################

# Figure 2 Data, Sensitivity to the number of initial rooms 
# Variables as follows: input: RoomHesh6.txt, UseLife = 25, S.D. of UseLife = 0.0, InitRooms = 12, 13, 14
dp.Fig2 <- read.csv('Fig2_ROOMHESH6_P.CSV')

# Plot Figure 2: Sensitivity to the number of initial rooms, Heshotauthla survey
png("Fig2.png",width=6.5,height=4,units="in",res=256)
  dp.Fig2 %>% 
    ggplot(aes(x=Period, y=Rate, color=as.factor(InitRm))) +
    geom_line(size=0.75) +
    scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
    scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
    theme_bw() + 
    theme(legend.position = c(0.01,0.99), 
          legend.justification = c(0, 1), 
          legend.text=element_text(size=6), 
          legend.title=element_text(size=8)) +
    scale_color_discrete(name = "Initial Rooms")
dev.off()

#########################
#########################

# Figure 3 Data, Sensitivity to Room Use-Life 
# Variables as follows: input: RoomHesh6.txt, UseLife = 15, 20, 25, 30, S.D. of UseLife = 0.0, InitRooms = 8, 10, 13, 15
dp.Fig3 <- read.csv('Fig3_ROOMHESH6_P.CSV')

# Plot Figure 3: Sensitivtiy to Room Use-Life, Heshotauthla Survey
png("Fig3.png",width=6.5,height=4,units="in",res=256)
dp.Fig3 %>% 
  ggplot(aes(x=Period, y=Rate, color=as.factor(UseLife))) +
  geom_line(size=0.75) +
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  theme_bw() + 
  theme(legend.position = c(0.01,0.99), 
        legend.justification = c(0, 1), 
        legend.text=element_text(size=6), 
        legend.title=element_text(size=8)) +
  scale_color_discrete(name = "Use Life")
dev.off()

#########################
#########################

# Figure 4 Data, Sensitivity to S.D. of Room Use-Life 
# Variables as follows: input: RoomHesh6.txt, UseLife = 25, S.D. of UseLife = 0, 5, 10, 15, 20, InitRooms = 13
dp.Fig4 <- read.csv('Fig4_ROOMHESH6_P.CSV')

# Plot Figure 4: Modeled growth rates using normally distributed structure use lives with mean of 25 years and different standard deviations (standard deviation of 0 is a fixed structure use life).
png("Fig4.png",width=6.5,height=4,units="in",res=256)
dp.Fig4 %>% 
  ggplot(aes(x=Period, y=Rate, color=as.factor(UseSD))) +
  geom_line(size=0.75) +
  scale_x_continuous(name='Period', breaks=seq(1,6,1)) + 
  scale_y_continuous(name='Growth Rate (%)', breaks=seq(-6,7,1)) +
  theme_bw() + 
  theme(legend.position = c(0.01,0.99), 
        legend.justification = c(0, 1), 
        legend.text=element_text(size=6), 
        legend.title=element_text(size=8)) +
  scale_color_discrete(name = "Use Life S.D.")
dev.off()

#########################
#########################

# Figure 5 Data, Number of Occupied Rooms by approach
# Formula derived as explained in text
# Simulated derived with Variables as follows: input: RoomHesh6.txt, UseLife = 25, S.D. of UseLife = 0.0, InitRooms = 13
dp.Fig5 <- read.csv('Fig5_ImpliedDSh.csv')

# Plot Figure 5: Number of occupied rooms through time for the Heshotauthla survey, computed annually for the simulated and using the formula 
png("Fig5.png",width=6.5,height=4,units="in",res=256)
dp.Fig5 %>% 
  ggplot(aes(x=Year, y=OccRm, color=as.factor(Group))) +
  geom_line(size=0.75) +
  scale_x_continuous(name='Year', breaks=seq(900,1275,25)) + 
  scale_y_continuous(name='Occupied Rooms', breaks=seq(0,250,25)) +
  theme_bw() + 
  theme(legend.position = c(0.01,0.99), 
        legend.justification = c(0, 1), 
        legend.text=element_text(size=6), 
        legend.title=element_text(size=8)) +
  scale_color_discrete(name = "Method")
dev.off()

#########################
#########################

# Figure 6 Data, Growth Rates by period for simulated and and compound interest methods of calculation 
# Variables as follows: input: RoomObap.txt, UseLife = 25, S.D. of UseLife = 0.0, InitRooms = 27
ds.Fig6 <- read.csv('Fig6_OBAPRates.csv')

# Plot Figure 6: Estimated growth rates for the Ojo Bonito survey derived from the formula and the simulation 
png("Fig6.png",width=6.5,height=4,units="in",res=256)
ds.Fig6 %>%
  ggplot(aes(x=Year, y=Rate, group=Approach, color=Approach)) +
  geom_line(size=0.75) +
  scale_x_continuous(name="Year", breaks=seq(1000,1275,25)) + 
  scale_y_continuous(name="Growth Rate (%)", breaks=seq(-6,7,1)) +
  theme_bw() +
  theme(legend.position = c(0.01,0.99), 
        legend.justification = c(0, 1), 
        legend.text=element_text(size=6), 
        legend.title=element_text(size=8)) +
  geom_hline(yintercept = 0) +
  scale_color_discrete(name = "Growth Rates")
dev.off()  

#########################
#########################

## Figure 7 Data, Number of Occupied Rooms by approach
# Formula derived as explained in text
# Simulated derived with Variables as follows: input: RoomHesh6.txt, UseLife = 25, S.D. of UseLife = 0.0, InitRooms = 27
dp.Fig7 <- read.csv('Fig7_ImpliedDSo.csv')

# Plot Figure 7: Number of occupied rooms through time for the Ojo Bonito survey, computed annually for the simulated and using the formula.
png("Fig7.png",width=6.5,height=4,units="in",res=256)
dp.Fig7 %>% 
  ggplot(aes(x=Year, y=OccRm, color=as.factor(Group))) +
  geom_line(size=0.75) +
  scale_x_continuous(name='Year', breaks=seq(1000,1275,25)) + 
  scale_y_continuous(name='Occupied Rooms', breaks=seq(0,400,25)) +
  theme_bw() + 
  theme(legend.position = c(0.01,0.99), 
        legend.justification = c(0, 1), 
        legend.text=element_text(size=6), 
        legend.title=element_text(size=8)) +
  scale_color_discrete(name = "Method")
dev.off()