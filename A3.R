setwd("~/Desktop/A3")

crime_long = read.csv("crime_long.csv")
officers = read.csv("officers.csv")
population = read.csv("population.csv")


# EX2
# plot
library("tidyr")
library("tidyverse")
library("reshape2")
library('dplyr')
crime_month <- aggregate(crime_long$crimes, by=list(crime_long$crime_month), sum)
ggplot(data=crime_month, aes(x=Group.1, y=x, group=1)) +
  geom_line()

# merge
Merge <- left_join(population,crime_long,by=c("month"="crime_month","district"="district"))

# panel data
crime_tot <- dcast(crime_long,crime_month + district ~ crime_type,sum,value.var = "crimes")

Panel_before <- left_join(population,crime_tot,by=c("month"="crime_month","district"="district"))
Panel_before$crime <- rowSums(Panel_before[,9:12])
Panel_before$crime_per <- Panel_before$crime / Panel_before$tot_pop
Panel_before$violent_per <- Panel_before$violent / Panel_before$tot_pop
Panel_before$property_per <- Panel_before$property / Panel_before$tot_pop
Panel_before$share_black <- Panel_before$tot_black / Panel_before$tot_pop
Panel_before$share_hisp <- Panel_before$tot_hisp / Panel_before$tot_pop
Panel_before$share_white <- Panel_before$tot_white / Panel_before$tot_pop

Panel=select(Panel_before,"month","district","period","crime", "crime_per","violent_per","property_per","p50_inc","share_black","share_hisp","share_white")

# EX3
officer_panel <- left_join(officers,Panel,by=c("unit"="district","month"="month"))
lm1 <- lm(arrest ~ tenure + crime + p50_inc + share_black + share_hisp + share_white, data = officer_panel)
summary(lm1)

# EX4
lm2 <- lm(arrest ~ tenure + crime + p50_inc + share_black + share_hisp + share_white + factor(unit) + factor(period), data = officer_panel)
summary(lm2)

# EX5
# between
officer_panel1 <- officer_panel[,c('NUID', 'tenure', 'arrest', 'crime', 'p50_inc', 'share_black', 'share_white', 'share_hisp')]
officer_panel2 <- aggregate(officer_panel1[2:8], by=list(officer_panel1$NUID), mean)
officer_panel3 <- officer_panel2
colnames(officer_panel3)[colnames(officer_panel3) %in% c("tenure", "arrest", "crime", "p50_inc", "share_black", "share_white", "share_hisp")] <- c("tenure_mean", "arrest_mean", "crime_mean", "p50_inc_mean", "share_black_mean", "share_white_mean", "share_hisp_mean")
officer_panel4 <- left_join(officer_panel, officer_panel3, by=c("NUID"="Group.1"))

lm3 <- lm(arrest_mean ~ tenure_mean + crime_mean + p50_inc_mean + share_black_mean + share_hisp_mean + share_white_mean + factor(unit) + factor(period), data = officer_panel4)
summary(lm3)

# within
officer_panel4$tenure1 <- officer_panel4$tenure - officer_panel4$tenure_mean
officer_panel4$arrest1 <- officer_panel4$arrest - officer_panel4$arrest_mean
officer_panel4$crime1 <- officer_panel4$crime - officer_panel4$crime_mean
officer_panel4$p50_inc1 <- officer_panel4$p50_inc - officer_panel4$p50_inc_mean
officer_panel4$share_black1 <- officer_panel4$share_black - officer_panel4$share_black_mean
officer_panel4$share_white1 <- officer_panel4$share_white - officer_panel4$share_white_mean
officer_panel4$share_hisp1 <- officer_panel4$share_hisp - officer_panel4$share_hisp_mean

officer_panel5 <- officer_panel4[, c('NUID','unit', 'period', 'tenure1', 'arrest1', 'crime1', 'p50_inc1', 'share_black1', 'share_white1', 'share_hisp1')]
lm4 <- lm(arrest1 ~ tenure1 + crime1 + p50_inc1 + share_black1 + share_hisp1 + share_white1 + factor(unit) + factor(period), data = officer_panel5)
summary(lm4)


