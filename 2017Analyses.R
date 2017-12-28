# Analyses in 2017
require("easypackages") # for loading and installing many packages at one time
packages_to_load <- c("broom", "dplyr", "tidyverse", "corrplot", "ggplot2", "GGally", "devtools", "ggthemes")
packages(packages_to_load, prompt = TRUE) # lock'n'load install/load


Kuitub <- read.table(file = "d1alln2017.txt",
                     header = TRUE,
                     dec = ".")

#Naming the factor levels
#Turning to factors
Kuitub$lan=factor(Kuitub$lan)
Kuitub$ker=factor(Kuitub$ker)
Kuitub$time=factor(Kuitub$time)
Kuitub$TOCN=factor(Kuitub$TOCN)
#Int to num
Kuitub$Smgkg <- as.numeric(as.character(Kuitub$Smgkg))


#Giving labes kas
Kuitub$lan <- factor(Kuitub$lan,
                     levels = c(1,2,3,4,5,6),
                     labels = c("S1", "S2", "S3","S4", "Min", "C"))

#Giving labes kas
Kuitub$kas <- factor(Kuitub$kas,
                     levels = c(1,2,3,4),
                     labels = c("R", "KK", "P","AK"))
#Giving labes time
Kuitub$time <- factor(Kuitub$time,
                      levels = c(1,2,3,4,5,6),
                      labels = c("29.9.2015", "23.10.2015", "15.11.2015", "22.4.2016", "12.9.2016", "xx.10.2017"))

ggpairs(
  Kuitub, 5:7,
  lower = list(combo = wrap("facethist", binwidth = 0.5)))

p <- ggplot(Kuitub, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan)) +theme_tufte()+geom_rangeframe(color="black")

Kuitub2 = Kuitub[ which(Kuitub$time=="xx.10.2017"), ]
p <- ggplot(Kuitub2, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan)) +theme_tufte()+geom_rangeframe(color="black")

###############################################################################
# AOV

Kuitub1 = Kuitub[ which(Kuitub$time=='29.9.2015'), ]
p <- ggplot(Kuitub1, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
           data = Kuitub1)

fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
            data = Kuitub1)
fitm1 <- aov(nitN ~  KuitLisKokN + KuitLisN + TOCN + TOCNliu+ LisC,
           data = Kuitub1)


