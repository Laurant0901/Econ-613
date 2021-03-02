
rm(list = ls())

setwd("~/Desktop")

datjss = read.csv("datjss.csv")
datsss = read.csv("datsss.csv")
datstu = read.csv("datstu.csv")

dim(datstu)

dim(datsss)

# exercise 1

# number of students
length(unique(datstu$X))

# number of schools
length(unique(datsss$schoolname))

# number of programs
pgm = datstu[!duplicated(datstu$choicepgm1), ]
dim(pgm)

# number of choices
datstu$choice1 <- paste(datstu$schoolcode1, "-", datstu$choicepgm1)
datstu$choice2 <- paste(datstu$schoolcode2, "-", datstu$choicepgm2)
datstu$choice3 <- paste(datstu$schoolcode3, "-", datstu$choicepgm3)
datstu$choice4 <- paste(datstu$schoolcode4, "-", datstu$choicepgm4)
datstu$choice5 <- paste(datstu$schoolcode5, "-", datstu$choicepgm5)
datstu$choice6 <- paste(datstu$schoolcode6, "-", datstu$choicepgm6)
choice = cbind(datstu$choice1, datstu$choice2, datstu$choice3, datstu$choice4, datstu$choice5, datstu$choice6)
choice %>%
  pivot_longer(
    cols = starts_with("choice"),
    names_to = "list",
    names_prefix = "choice",
    values_to = "choice",
    values_drop_na = TRUE
  )
choice = choice[choice!=""]
length(unique(choice))

# missing test score
sum(is.na(datstu$score))

# Ex2

library("dplyr")
library("tidyr")
schchoice <- datstu %>%
  select(-c(5:16)) %>%
  pivot_longer(
    cols = starts_with("choice"),
    names_to = "list",
    names_prefix = "choice",
    values_to = "choice",
    values_drop_na = TRUE
  ) %>%
  filter(rankplace == list) %>%
  separate(choice, c("schoolcode", "program"), sep = " - ")

sss <- datsss %>%
  select(c(3:6))
unique_school = sss[!duplicated(sss$schoolcode), ]

schcho_sss <- merge(x = schchoice, y = sss, by = "schoolcode", all.x = TRUE)
schcho_sss1 = group_by(schcho_sss, schoolcode)
dfschcho = data_frame(summarise(schcho_sss1, cutoff = min(score), quality = mean(score), size=n()))

school_level = merge(schcho_sss1, dfschcho, by = 'schoolcode', all.x = T, all.y = T)
school_level1 = subset(school_level, select = -c(X, agey, male, jssdistrict, rankplace) )
school_level2 = school_level1[!duplicated(school_level1[c("schoolcode", "program")]), ]

school_levelFin = subset(school_level2, select = -c(score) )
school_levelFin[1:20, ]

# Ex3
school_mapping <- merge(school_levelFin,datjss,by=c("jssdistrict"))

# replace with zeros
school_mapping[is.na(school_mapping)] <- 0#4

# element wise thing
school_mapping$distance = sqrt(69.172*(school_mapping$ssslong-school_mapping$point_x)*cos(school_mapping$point_y/57.3)^2+(69.172*(school_mapping$ssslat-school_mapping$point_y))^2)
school_mapping$distance

#Ex4
jss = datjss %>%
  select(c(2:4))
schcho_sss2 <- merge(x = school_level, y = jss, by = "jssdistrict", all.x = TRUE)
schcho_sss3 = group_by(schcho_sss2, schoolcode)
cutoff_mean <- mean(schcho_sss3$cutoff)
cutoff_mean
quality_mean <- mean(schcho_sss3$quality)
quality_mean
distance <- mean(school_mapping$distance, na.rm=TRUE)
distance
cutoff_stdev <- sd(schcho_sss3$cutoff)
cutoff_stdev
quality_stdev <- sd(schcho_sss3$quality)
quality_stdev
distance_stdev <- sd(school_mapping$distance, na.rm=TRUE)
distance_stdev
