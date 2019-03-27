#Dc_stranding

# tomo eguchi
# 7 January 2015


rm(list=ls())
library(ggplot2)
runDate <- Sys.Date()
save.fig <- F

# For 2015 report: 'data/WC_DC_StrandingsJan2015.csv'
#infile <- 'data/WC_DC_StrandingsJan2017.csv'
infile <- 'data/WC_DC_Strandings_MAR2019.csv'

dat1 <- read.table(infile, sep = ",", header = TRUE) %>%
  mutate(yr.fac = factor(Year_Initially_Observed))
#dat1$yr.fac <- factor(dat1$Year_Initially_Observed)

dat1.dead <- dat1 %>% filter(is.na(Alive_Released))
#dat1.dead.CA <- dat1.dead[dat1.dead$STATE == 'CA',]
dat1.human <- dat1.dead %>% 
  filter(Human_Interaction != "NO") #[dat1.dead$Human_Interaction != 'NO',]
# dat1.fishery <- dat1.human[dat1.human$HUMAN_INTERACTION_CAUSE != 'BOAT COLLISION' &
#                              dat1.human$HUMAN_INTERACTION_CAUSE != 'OTHER' &
#                              dat1.human$HUMAN_INTERACTION_CAUSE != 'POWER PLANT ENTRAINMENT' &
#                              dat1.human$HUMAN_INTERACTION_CAUSE != 'SHOT', ]

#dat1.fishery <- dat1.human[dat1.human$Fishery_Interaction == T,]

summary(dat1.human)
# change the colors by state. 

p1 <- ggplot(data = dat1.human) +
  geom_bar(aes(x = yr.fac, fill = State)) + 
  #qplot(yr.fac, data = dat1.fishery, geom = "bar", fill = STATE) + 
  scale_y_continuous(breaks = seq(0, 17, 1)) +
  ylab('Counts') + xlab('Year') + 
  ggtitle('Stranded leatherback turtles with human interaction') +
  theme(axis.text.x = element_text(angle = 90, size = 15, vjust = 0.5))

p2 <- ggplot(data = dat1.dead) +
  geom_bar(aes(x = yr.fac, fill = State)) + 
  #qplot(yr.fac, data = dat1.fishery, geom = "bar", fill = STATE) + 
  scale_y_continuous(breaks = seq(0, 17, 1)) +
  ylab('Counts') + xlab('Year') + 
  ggtitle('Stranded leatherback turtles') +
  theme(axis.text.x = element_text(angle = 90, size = 15, vjust = 0.5))

if (save.fig){
  ggsave(filename = paste0('figures/Dc_strandings_', 
                           runDate, '.png'),
         plot = p2,
         width = 8,
         height = 7,
         dpi = 600)
  
}

