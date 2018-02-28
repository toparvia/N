# ANOVA TUKEY ANALYSES

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
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #This did not work
  #I need to put the labels in the same order as in the boxplot :
  #Tukey.labels$variable=rownames(Tukey.labels)
  #Tukey.labels=Tukey.labels[order(Tukey.labels$variable) , ]
  #Tukey.labels=Tukey.labels[order(Klabels) , ]
  return(Tukey.labels)
}

#Subsets
#R
Kuitub29.9.2015R  = Kuitub[ which(Kuitub$time=='29.9.2015'  & Kuitub$kas == "R" ), ]
Kuitub23.10.2015R = Kuitub[ which(Kuitub$time=='23.10.2015' & Kuitub$kas == "R" ), ]
Kuitub15.11.2015R = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "R" ), ]
Kuitub22.4.2016R  = Kuitub[ which(Kuitub$time=='22.4.2016'  & Kuitub$kas == "R" ), ]
Kuitub12.9.2016R  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "R" ), ]
Kuitub16.10.2017R = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "R" ), ]

#Statistics
#Anova
AKuitub29.9.2015R <- aov(nitN ~ factor(lan) + factor(ker),
           data = Kuitub29.9.2015R)
AKuitub23.10.2015R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub23.10.2015R)
AKuitub15.11.2015R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub15.11.2015R)
AKuitub22.4.2016R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub22.4.2016R)
AKuitub12.9.2016R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub12.9.2016R)
AKuitub16.10.2017R <- aov(nitN ~ factor(lan) + factor(ker),
                         data = Kuitub16.10.2017R)
#Tukey
TKuitub29.9.2015R <- TukeyHSD(AKuitub29.9.2015R, "factor(lan)", conf.level = 0.95)
TKuitub23.10.2015R <- TukeyHSD(AKuitub23.10.2015R, "factor(lan)", conf.level = 0.95)
TKuitub15.11.2015R <- TukeyHSD(AKuitub15.11.2015R, "factor(lan)", conf.level = 0.95)
TKuitub22.4.2016R <- TukeyHSD(AKuitub22.4.2016R, "factor(lan)", conf.level = 0.95)
TKuitub12.9.2016R <- TukeyHSD(AKuitub12.9.2016R, "factor(lan)", conf.level = 0.95)
TKuitub16.10.2017R <- TukeyHSD(AKuitub16.10.2017R, "factor(lan)", conf.level = 0.95)

Klabels = c("S1", "S2", "S3","S4", "Min", "C")
#Labels
Label29.9.2015R <-generate_label_df(TKuitub29.9.2015R, "factor(lan)")
Label23.10.2015R <-generate_label_df(TKuitub23.10.2015R, "factor(lan)")
Label15.11.2015R <-generate_label_df(TKuitub15.11.2015R, "factor(lan)")
Label22.4.2016R <-generate_label_df(TKuitub22.4.2016R, "factor(lan)")
Label12.9.2016R <-generate_label_df(TKuitub12.9.2016R, "factor(lan)")
Label16.10.2017R <-generate_label_df(TKuitub16.10.2017R, "factor(lan)")
