# ANOVA TUKEY ANALYSES

require("easypackages") # for loading and installing many packages at one time
packages_to_load <- c("broom", "dplyr", "plyr", "tidyverse", "corrplot", "ggplot2", "GGally", "devtools", "ggthemes","multcompView")
packages(packages_to_load, prompt = TRUE) # lock'n'load install/load


Kuitub <- read.table(file = "Nall_27.2.2018.txt",
                     header = TRUE,
                     dec = ".")

################################################################################
#Setup

#Naming the factor levels
#Turning to factors
#Kuitub$lan=factor(Kuitub$lan)
Kuitub$ker=factor(Kuitub$ker)
#Kuitub$kas=factor(Kuitub$kas)
Kuitub$time=factor(Kuitub$time)
#Kuitub$TOCN=factor(Kuitub$TOCN)
#Int to num
Kuitub$Smgkg <- as.numeric(as.character(Kuitub$Smgkg))
Kuitub$KuitLisKokN <- as.numeric(Kuitub$KuitLisKokN)
Kuitub$KuitLisN <- as.numeric(Kuitub$KuitLisN)
Kuitub$TOCN <- as.numeric(Kuitub$TOCN)
Kuitub$TOCNliu <- as.numeric(Kuitub$TOCNliu)
Kuitub$LisC <- as.numeric(Kuitub$LisC)

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
                      labels = c("29.9.2015", "23.10.2015", "15.11.2015", "22.4.2016", "12.9.2016", "16.10.2017"))

###################################################################################################################
#Functions

#This function will generate the labels Tukey Honest Significance for the selected variables
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order :
  #First solution, gives C, Min, S1 order...
  #Tukey.labels$variable=rownames(Tukey.labels)
  #Tukey.labels=Tukey.labels[order(Tukey.labels$variable, decreasing = FALSE) , ]
  #Second Solution
  Tukey.labels$variable=rownames(Tukey.labels)
  labelslan = c("S1", "S2", "S3","S4", "Min", "C")
  Tukey.labels=Tukey.labels[order(match(Tukey.labels$variable,labelslan)),]
  
  return(Tukey.labels)
}



# example to show how ordering part of the function is build
# First solution, gives C, Min, S1 order...
# TKuitub29.9.2015$`factor(lan)`[order(TKuitub29.9.2015[["factor(lan)"]][,4],decreasing = FALSE)]
# Second Solution
# labelslan = c("S1", "S2", "S3","S4", "Min", "C")
# Label29.9.2015[order(match(Label29.9.2015$variable,labelslan)),]


############################################################################
#Subsets
#all plants
Kuitub29.9.2015  = Kuitub[ which(Kuitub$time=='29.9.2015')  , ]
Kuitub23.10.2015 = Kuitub[ which(Kuitub$time=='23.10.2015') , ]
Kuitub15.11.2015 = Kuitub[ which(Kuitub$time=='15.11.2015') , ]
Kuitub22.4.2016  = Kuitub[ which(Kuitub$time=='22.4.2016')  , ]
Kuitub12.9.2016  = Kuitub[ which(Kuitub$time=='12.9.2016')  , ]
Kuitub16.10.2017 = Kuitub[ which(Kuitub$time=='16.10.2017') , ]

#Plants seperately labels = c("R", "KK", "P","AK"))
#R
Kuitub29.9.2015R  = Kuitub[ which(Kuitub$time=='29.9.2015'  & Kuitub$kas == "R" ), ]
Kuitub23.10.2015R = Kuitub[ which(Kuitub$time=='23.10.2015' & Kuitub$kas == "R" ), ]
Kuitub15.11.2015R = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "R" ), ]
Kuitub22.4.2016R  = Kuitub[ which(Kuitub$time=='22.4.2016'  & Kuitub$kas == "R" ), ]
Kuitub12.9.2016R  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "R" ), ]
Kuitub16.10.2017R = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "R" ), ]

#KK
Kuitub29.9.2015KK  = Kuitub[ which(Kuitub$time=='29.9.2015'  & Kuitub$kas == "KK" ), ]
Kuitub23.10.2015KK = Kuitub[ which(Kuitub$time=='23.10.2015' & Kuitub$kas == "KK" ), ]
Kuitub15.11.2015KK = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "KK" ), ]
Kuitub22.4.2016KK  = Kuitub[ which(Kuitub$time=='22.4.2016'  & Kuitub$kas == "KK" ), ]
Kuitub12.9.2016KK  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "KK" ), ]
Kuitub16.10.2017KK = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "KK" ), ]

#P
Kuitub29.9.2015P  = Kuitub[ which(Kuitub$time=='29.9.2015'  & Kuitub$kas == "P" ), ]
Kuitub23.10.2015P = Kuitub[ which(Kuitub$time=='23.10.2015' & Kuitub$kas == "P" ), ]
Kuitub15.11.2015P = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "P" ), ]
Kuitub22.4.2016P  = Kuitub[ which(Kuitub$time=='22.4.2016'  & Kuitub$kas == "P" ), ]
Kuitub12.9.2016P  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "P" ), ]
Kuitub16.10.2017P = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "P" ), ]

#AK
Kuitub29.9.2015AK  = Kuitub[ which(Kuitub$time=='29.9.2015'  & Kuitub$kas == "AK" ), ]
Kuitub23.10.2015AK = Kuitub[ which(Kuitub$time=='23.10.2015' & Kuitub$kas == "AK" ), ]
Kuitub15.11.2015AK = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "AK" ), ]
Kuitub22.4.2016AK  = Kuitub[ which(Kuitub$time=='22.4.2016'  & Kuitub$kas == "AK" ), ]
Kuitub12.9.2016AK  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "AK" ), ]
Kuitub16.10.2017AK = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "AK" ), ]

############################################################################
#Statistics
############################################################################
# All plants
# Anova
AKuitub29.9.2015 <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub29.9.2015)
AKuitub23.10.2015 <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub23.10.2015)
AKuitub15.11.2015 <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub15.11.2015)
AKuitub22.4.2016 <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub22.4.2016)
AKuitub12.9.2016 <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub12.9.2016)
AKuitub16.10.2017 <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub16.10.2017)

#Tukey
TKuitub29.9.2015 <- TukeyHSD(AKuitub29.9.2015,   "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015 <- TukeyHSD(AKuitub23.10.2015, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015 <- TukeyHSD(AKuitub15.11.2015, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016 <- TukeyHSD(AKuitub22.4.2016,   "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016 <- TukeyHSD(AKuitub12.9.2016,   "factor(lan)", conf.level = 0.95)
TKuitub16.10.2017 <- TukeyHSD(AKuitub16.10.2017, "factor(lan)", conf.level = 0.95)
#Group differences, labels
#Labels
Label29.9.2015 <-generate_label_df(TKuitub29.9.2015,   "factor(lan)")
Label23.10.2015 <-generate_label_df(TKuitub23.10.2015, "factor(lan)")
Label15.11.2015 <-generate_label_df(TKuitub15.11.2015, "factor(lan)")
Label22.4.2016 <-generate_label_df(TKuitub22.4.2016,   "factor(lan)")
Label12.9.2016 <-generate_label_df(TKuitub12.9.2016,   "factor(lan)")
Label16.10.2017 <-generate_label_df(TKuitub16.10.2017, "factor(lan)")

############################################################################

# Lannoitus * kasvi
# Anova
AKuitub29.9.2015Mult <- aov(nitN ~ factor(lan) * factor(kas) + factor(ker),
                            data = Kuitub29.9.2015)
AKuitub23.10.2015Mult <- aov(nitN ~ factor(lan) *factor(kas) + factor(ker),
                             data = Kuitub23.10.2015)
AKuitub15.11.2015Mult <- aov(nitN ~ factor(lan) *factor(kas) + factor(ker),
                             data = Kuitub15.11.2015)
AKuitub22.4.2016Mult <- aov(nitN ~ factor(lan) * factor(kas) + factor(ker),
                            data = Kuitub22.4.2016)
AKuitub12.9.2016Mult <- aov(nitN ~ factor(lan) * factor(kas) + factor(ker),
                            data = Kuitub12.9.2016)
AKuitub16.10.2017Mult <- aov(nitN ~ factor(lan) *factor(kas) + factor(ker),
                             data = Kuitub16.10.2017)

#Tukey
TKuitub29.9.2015Mult <- TukeyHSD(AKuitub29.9.2015Mult,   "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub23.10.2015Mult <- TukeyHSD(AKuitub23.10.2015Mult, "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub15.11.2015Mult <- TukeyHSD(AKuitub15.11.2015Mult, "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub22.4.2016Mult <- TukeyHSD(AKuitub22.4.2016Mult,   "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub12.9.2016Mult <- TukeyHSD(AKuitub12.9.2016Mult,   "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub16.10.2017Mult <- TukeyHSD(AKuitub16.10.2017Mult, "factor(lan):factor(kas)", conf.level = 0.95)
#Group differences, labels
#Labels
Label29.9.2015Mult <-generate_label_df(TKuitub29.9.2015Mult,   "factor(lan):factor(kas)")
Label23.10.2015Mult <-generate_label_df(TKuitub23.10.2015Mult, "factor(lan):factor(kas)")
Label15.11.2015Mult <-generate_label_df(TKuitub15.11.2015Mult, "factor(lan):factor(kas)")
Label22.4.2016Mult <-generate_label_df(TKuitub22.4.2016Mult,   "factor(lan):factor(kas)")
Label12.9.2016Mult <-generate_label_df(TKuitub12.9.2016Mult,   "factor(lan):factor(kas)")
Label16.10.2017Mult <-generate_label_df(TKuitub16.10.2017Mult, "factor(lan):factor(kas)")

############################################################################
# R plants
#Anova
AKuitub29.9.2015R <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub29.9.2015R)
AKuitub23.10.2015R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub23.10.2015R)
AKuitub15.11.2015R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub15.11.2015R)
AKuitub22.4.2016R <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub22.4.2016R)
AKuitub12.9.2016R <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub12.9.2016R)
AKuitub16.10.2017R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub16.10.2017R)
#Tukey
TKuitub29.9.2015R <- TukeyHSD(AKuitub29.9.2015R,   "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015R <- TukeyHSD(AKuitub23.10.2015R, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015R <- TukeyHSD(AKuitub15.11.2015R, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016R <- TukeyHSD(AKuitub22.4.2016R,   "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016R <- TukeyHSD(AKuitub12.9.2016R,   "factor(lan)", conf.level = 0.95)
TKuitub16.10.2017R <- TukeyHSD(AKuitub16.10.2017R, "factor(lan)", conf.level = 0.95)

#Labels
Label29.9.2015R <-generate_label_df(TKuitub29.9.2015R,   "factor(lan)")
Label23.10.2015R <-generate_label_df(TKuitub23.10.2015R, "factor(lan)")
Label15.11.2015R <-generate_label_df(TKuitub15.11.2015R, "factor(lan)")
Label22.4.2016R <-generate_label_df(TKuitub22.4.2016R,   "factor(lan)")
Label12.9.2016R <-generate_label_df(TKuitub12.9.2016R,   "factor(lan)")
Label16.10.2017R <-generate_label_df(TKuitub16.10.2017R, "factor(lan)")

# KK plants
#Anova
AKuitub29.9.2015KK <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub29.9.2015KK)
AKuitub23.10.2015KK <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub23.10.2015KK)
AKuitub15.11.2015KK <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub15.11.2015KK)
AKuitub22.4.2016KK <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub22.4.2016KK)
AKuitub12.9.2016KK <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub12.9.2016KK)
AKuitub16.10.2017KK <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub16.10.2017KK)
#Tukey
TKuitub29.9.2015KK <- TukeyHSD(AKuitub29.9.2015KK,   "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015KK <- TukeyHSD(AKuitub23.10.2015KK, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015KK <- TukeyHSD(AKuitub15.11.2015KK, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016KK <- TukeyHSD(AKuitub22.4.2016KK,   "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016KK <- TukeyHSD(AKuitub12.9.2016KK,   "factor(lan)", conf.level = 0.95)
TKuitub16.10.2017KK <- TukeyHSD(AKuitub16.10.2017KK, "factor(lan)", conf.level = 0.95)

#Labels
Label29.9.2015KK <-generate_label_df(TKuitub29.9.2015KK,   "factor(lan)")
Label23.10.2015KK <-generate_label_df(TKuitub23.10.2015KK, "factor(lan)")
Label15.11.2015KK <-generate_label_df(TKuitub15.11.2015KK, "factor(lan)")
Label22.4.2016KK <-generate_label_df(TKuitub22.4.2016KK,   "factor(lan)")
Label12.9.2016KK <-generate_label_df(TKuitub12.9.2016KK,   "factor(lan)")
Label16.10.2017KK <-generate_label_df(TKuitub16.10.2017KK, "factor(lan)")

# P plants
#Anova
AKuitub29.9.2015P <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub29.9.2015P)
AKuitub23.10.2015P <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub23.10.2015P)
AKuitub15.11.2015P <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub15.11.2015P)
AKuitub22.4.2016P <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub22.4.2016P)
AKuitub12.9.2016P <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub12.9.2016P)
AKuitub16.10.2017P <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub16.10.2017P)
#Tukey
TKuitub29.9.2015P <- TukeyHSD(AKuitub29.9.2015P,   "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015P <- TukeyHSD(AKuitub23.10.2015P, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015P <- TukeyHSD(AKuitub15.11.2015P, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016P <- TukeyHSD(AKuitub22.4.2016P,   "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016P <- TukeyHSD(AKuitub12.9.2016P,   "factor(lan)", conf.level = 0.95)
TKuitub16.10.2017P <- TukeyHSD(AKuitub16.10.2017P, "factor(lan)", conf.level = 0.95)

#Labels
Label29.9.2015P <-generate_label_df(TKuitub29.9.2015P,   "factor(lan)")
Label23.10.2015P <-generate_label_df(TKuitub23.10.2015P, "factor(lan)")
Label15.11.2015P <-generate_label_df(TKuitub15.11.2015P, "factor(lan)")
Label22.4.2016P <-generate_label_df(TKuitub22.4.2016P,   "factor(lan)")
Label12.9.2016P <-generate_label_df(TKuitub12.9.2016P,   "factor(lan)")
Label16.10.2017P <-generate_label_df(TKuitub16.10.2017P, "factor(lan)")

# AK plants
#Anova
AKuitub29.9.2015AK <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub29.9.2015AK)
AKuitub23.10.2015AK <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub23.10.2015AK)
AKuitub15.11.2015AK <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub15.11.2015AK)
AKuitub22.4.2016AK <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub22.4.2016AK)
AKuitub12.9.2016AK <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub12.9.2016AK)
AKuitub16.10.2017AK <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub16.10.2017AK)
#Tukey
TKuitub29.9.2015AK <- TukeyHSD(AKuitub29.9.2015AK,   "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015AK <- TukeyHSD(AKuitub23.10.2015AK, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015AK <- TukeyHSD(AKuitub15.11.2015AK, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016AK <- TukeyHSD(AKuitub22.4.2016AK,   "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016AK <- TukeyHSD(AKuitub12.9.2016AK,   "factor(lan)", conf.level = 0.95)
TKuitub16.10.2017AK <- TukeyHSD(AKuitub16.10.2017AK, "factor(lan)", conf.level = 0.95)

#Labels
Label29.9.2015AK <-generate_label_df(TKuitub29.9.2015AK,   "factor(lan)")
Label23.10.2015AK <-generate_label_df(TKuitub23.10.2015AK, "factor(lan)")
Label15.11.2015AK <-generate_label_df(TKuitub15.11.2015AK, "factor(lan)")
Label22.4.2016AK <-generate_label_df(TKuitub22.4.2016AK,   "factor(lan)")
Label12.9.2016AK <-generate_label_df(TKuitub12.9.2016AK,   "factor(lan)")
Label16.10.2017AK <-generate_label_df(TKuitub16.10.2017AK, "factor(lan)")

############################################################################
#Generating the table for all Tukey HSD differences at confidence level 0.95

Timelabels = c("29.9.2015", "23.10.2015", "15.11.2015", "22.4.2016", "12.9.2016", "16.10.2017")

#Combining the labels to table. Order: labels = c("R", "KK", "P","AK"))
TukeyDifferencesR <- cbind(
  Label29.9.2015R[1],
  Label23.10.2015R[1],
  Label15.11.2015R[1],
  Label22.4.2016R[1],
  Label12.9.2016R[1],
  Label16.10.2017R[1] 
)

TukeyDifferencesKK <- cbind(
  Label29.9.2015KK[1],
  Label23.10.2015KK[1],
  Label15.11.2015KK[1],
  Label22.4.2016KK[1],
  Label12.9.2016KK[1],
  Label16.10.2017KK[1] 
)

TukeyDifferencesP <- cbind(
  Label29.9.2015P[1],
  Label23.10.2015P[1],
  Label15.11.2015P[1],
  Label22.4.2016P[1],
  Label12.9.2016P[1],
  Label16.10.2017P[1] 
)

TukeyDifferencesAK <- cbind(
  Label29.9.2015AK[1],
  Label23.10.2015AK[1],
  Label15.11.2015AK[1],
  Label22.4.2016AK[1],
  Label12.9.2016AK[1],
  Label16.10.2017AK[1] 
)

#Naming the tables
colnames(TukeyDifferencesR) <- Timelabels
colnames(TukeyDifferencesKK) <- Timelabels
colnames(TukeyDifferencesP) <- Timelabels
colnames(TukeyDifferencesAK) <- Timelabels

write.table(TukeyDifferencesR, file="rawresult/TukeyDifferencesR.csv")
write.table(TukeyDifferencesKK, file="rawresult/TukeyDifferencesKK.csv")
write.table(TukeyDifferencesP, file="rawresult/TukeyDifferencesP.csv")
write.table(TukeyDifferencesAK, file="rawresult/TukeyDifferencesAK.csv")

SummaryResults = ddply(Kuitub, .(kas, lan, time), function(x){
  c(mean=mean(x$nitN), sd = sd(x$nitN))
})

#SummaryResults #contains all nitN treatment means and standard deviations
#write.table(SummaryResults, "nitNmeans6times.csv", sep="\t")

SummaryResults$lower = SummaryResults$mean + SummaryResults$sd
SummaryResults$upper = SummaryResults$mean - SummaryResults$sd

write.table(SummaryResults, "rawresult/SummaryResultsNitrate.csv")

############################################################################

KuituR  = Kuitub[complete.cases(Kuitub), ]
SummaryResultsSulfate = ddply(KuituR, .(kas, lan, time), function(x){
  c(mean=mean(x$Smgkg), sd = sd(x$Smgkg))
})

#agg #contains all nitN treatment means and standard deviations
#write.table(agg, "nitNmeans6times.txt", sep="\t")

SummaryResultsSulfate$lower = SummaryResultsSulfate$mean + SummaryResultsSulfate$sd
SummaryResultsSulfate$upper = SummaryResultsSulfate$mean - SummaryResultsSulfate$sd

write.table(SummaryResultsSulfate, "rawresult/SummaryResultsSulfate.csv")

############################################################################
# SULFAATIT
############################################################################
# All plants
# Anova
AKuitub29.9.2015 <- aov(Smgkg ~ factor(lan) + factor(ker),
                        data = Kuitub29.9.2015)
AKuitub23.10.2015 <- aov(Smgkg ~ factor(lan) + factor(ker),
                         data = Kuitub23.10.2015)
AKuitub15.11.2015 <- aov(Smgkg ~ factor(lan) + factor(ker),
                         data = Kuitub15.11.2015)
AKuitub22.4.2016 <- aov(Smgkg ~ factor(lan) + factor(ker),
                        data = Kuitub22.4.2016)
AKuitub12.9.2016 <- aov(Smgkg ~ factor(lan) + factor(ker),
                        data = Kuitub12.9.2016)


#Tukey
TKuitub29.9.2015 <- TukeyHSD(AKuitub29.9.2015,   "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015 <- TukeyHSD(AKuitub23.10.2015, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015 <- TukeyHSD(AKuitub15.11.2015, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016 <- TukeyHSD(AKuitub22.4.2016,   "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016 <- TukeyHSD(AKuitub12.9.2016,   "factor(lan)", conf.level = 0.95)

#Group differences, labels
#Labels
Label29.9.2015 <-generate_label_df(TKuitub29.9.2015,   "factor(lan)")
Label23.10.2015 <-generate_label_df(TKuitub23.10.2015, "factor(lan)")
Label15.11.2015 <-generate_label_df(TKuitub15.11.2015, "factor(lan)")
Label22.4.2016 <-generate_label_df(TKuitub22.4.2016,   "factor(lan)")
Label12.9.2016 <-generate_label_df(TKuitub12.9.2016,   "factor(lan)")


# Lannoitus * kasvi
# Anova
AKuitub29.9.2015Mult <- aov(Smgkg ~ factor(lan) * factor(kas) + factor(ker),
                            data = Kuitub29.9.2015)
AKuitub23.10.2015Mult <- aov(Smgkg ~ factor(lan) *factor(kas) + factor(ker),
                             data = Kuitub23.10.2015)
AKuitub15.11.2015Mult <- aov(Smgkg ~ factor(lan) *factor(kas) + factor(ker),
                             data = Kuitub15.11.2015)
AKuitub22.4.2016Mult <- aov(Smgkg ~ factor(lan) * factor(kas) + factor(ker),
                            data = Kuitub22.4.2016)
AKuitub12.9.2016Mult <- aov(Smgkg ~ factor(lan) * factor(kas) + factor(ker),
                            data = Kuitub12.9.2016)

#Tukey
TKuitub29.9.2015Mult <- TukeyHSD(AKuitub29.9.2015Mult,   "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub23.10.2015Mult <- TukeyHSD(AKuitub23.10.2015Mult, "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub15.11.2015Mult <- TukeyHSD(AKuitub15.11.2015Mult, "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub22.4.2016Mult <- TukeyHSD(AKuitub22.4.2016Mult,   "factor(lan):factor(kas)", conf.level = 0.95)
TKuitub12.9.2016Mult <- TukeyHSD(AKuitub12.9.2016Mult,   "factor(lan):factor(kas)", conf.level = 0.95)

#Group differences, labels
#Labels
Label29.9.2015Mult <-generate_label_df(TKuitub29.9.2015Mult,   "factor(lan):factor(kas)")
Label23.10.2015Mult <-generate_label_df(TKuitub23.10.2015Mult, "factor(lan):factor(kas)")
Label15.11.2015Mult <-generate_label_df(TKuitub15.11.2015Mult, "factor(lan):factor(kas)")
Label22.4.2016Mult <-generate_label_df(TKuitub22.4.2016Mult,   "factor(lan):factor(kas)")
Label12.9.2016Mult <-generate_label_df(TKuitub12.9.2016Mult,   "factor(lan):factor(kas)")

#Pelkkä lannoituksen analyysi riittää

#R
AKuitub29.9.2015R <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub29.9.2015R)
AKuitub23.10.2015R <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub23.10.2015R)
AKuitub15.11.2015R <- aov(nitN ~ factor(lan) + factor(ker),
                          data = Kuitub15.11.2015R)
AKuitub22.4.2016R <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub22.4.2016R)
AKuitub12.9.2016R <- aov(nitN ~ factor(lan)  + factor(ker),
                         data = Kuitub22.4.2016R)

TKuitub29.9.2015R <- TukeyHSD(AKuitub29.9.2015R,   "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015R <- TukeyHSD(AKuitub23.10.2015R, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015R <- TukeyHSD(AKuitub15.11.2015R, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016R <- TukeyHSD(AKuitub22.4.2016R,   "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016R <- TukeyHSD(AKuitub12.9.2016R,   "factor(lan)", conf.level = 0.95)


#Labels
Label29.9.2015R <-generate_label_df(TKuitub29.9.2015R,   "factor(lan)")
Label23.10.2015R <-generate_label_df(TKuitub23.10.2015R, "factor(lan)")
Label15.11.2015R <-generate_label_df(TKuitub15.11.2015R, "factor(lan)")
Label22.4.2016R <-generate_label_df(TKuitub22.4.2016R,   "factor(lan)")
Label12.9.2016R <-generate_label_df(TKuitub12.9.2016R,   "factor(lan)")

 