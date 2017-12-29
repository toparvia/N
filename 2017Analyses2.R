# Analyses in 2017
require("easypackages") # for loading and installing many packages at one time
packages_to_load <- c("broom", "dplyr", "tidyverse", "corrplot", "ggplot2", "GGally", "devtools", "ggthemes")
packages(packages_to_load, prompt = TRUE) # lock'n'load install/load


Kuitub <- read.table(file = "d1alln2017.txt",
                     header = TRUE,
                     dec = ".")

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
                      labels = c("29.9.2015", "23.10.2015", "15.11.2015", "22.4.2016", "12.9.2016", "xx.10.2017"))

ggpairs(
  Kuitub, c(7,9,13,15),
  lower = list(combo = wrap("facethist", binwidth = 0.5)))

p <- ggplot(Kuitub, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan)) +theme_tufte()+geom_rangeframe(color="black") +
  ggtitle("All measurement average nitrate NO3- (mg/kg) with standard deviation")

Kuitub2 = Kuitub[ which(Kuitub$time=="xx.10.2017"), ]
p <- ggplot(Kuitub2, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan)) +theme_tufte()+geom_rangeframe(color="black") +
  ggtitle("xx.10.2017 Nitrate NO3- (mg/kg) means with standard deviation")

###############################################################################
# To look at the first year differences

Kuitub1 = Kuitub[ which(Kuitub$time=='29.9.2015'), ]
p <- ggplot(Kuitub1, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
           data = Kuitub1)


TukeyHSD(fit, "factor(kas)", ordered = TRUE)
TukeyHSD(fit, "factor(lan)", ordered = TRUE)

plot(TukeyHSD(fit, "factor(lan)", conf.level = 0.99))
plot(TukeyHSD(fit, "factor(kas)", conf.level = 0.99))

###
fitm1 <- lm(nitN ~  KuitLisKokN + KuitLisN + TOCN + TOCNliu+ LisC,
           data = Kuitub1)
# TOCN is best explaining the variation
# TOCN = Total organic carbon / total nitrogen in sludge

fitlm1 <- lm(nitN ~ kas*TOCN * KuitLisKokN,
             data = Kuitub1)
fitlm2 <- lm(nitN ~ kas*TOCN * KuitLisN,
             data = Kuitub1)
fitlm3 <- lm(formula = nitN ~ kas + TOCN + KuitLisKokN + kas:KuitLisKokN, data = Kuitub1)
# MASS::stepAIC(fitlm1)
# We can also explain variation without the lan, so we are now using the TOCN and added soluble nitrogen

AIC(fitlm1,fitlm2,fitlm3)

# model fitlm2 selected

summary(fitlm2)
#################################################################
Kuitub1 = Kuitub[ which(Kuitub$time=='15.11.2015'), ]
p <- ggplot(Kuitub1, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
           data = Kuitub1)

# means = 3 mean values calcualted from raw data
# se = 3 standard errors
# categories = 3 categorical / factors that group the data
# x.axis.label = what should be plotted on the x axis
# y.axis.label = what should be plotted on the y axis


#### The function STARTS here ####
plotTukeyHSD <- plotTukeysHSD <- function(tukey.out,
                                          x.axis.label = "Comparison",
                                          y.axis.label = "Effect Size",
                                          axis.adjust = 0,
                                          adjust.x.spacing = 5){
  
  tukey.out <- as.data.frame(tukey.out[[1]])
  means <- tukey.out$diff
  categories <- row.names(tukey.out)
  groups <- length(categories)
  ci.low <- tukey.out$lwr
  ci.up  <- tukey.out$upr                         
  
  n.means <- length(means)
  
  #determine where to plot points along x-axis
  x.values <- 1:n.means
  x.values <- x.values/adjust.x.spacing
  
  
  # calculate values for plotting limits            
  y.max <- max(ci.up) +                    
    max(ci.up)*axis.adjust
  y.min <- min(ci.low) - 
    max(ci.low)*axis.adjust
  
  if(groups == 2){ x.values <- c(0.25, 0.5)}
  if(groups == 3){ x.values <- c(0.25, 0.5,0.75)}
  
  x.axis.min <- min(x.values)-0.05
  x.axis.max <- max(x.values)+0.05
  
  x.limits <- c(x.axis.min,x.axis.max)
  
  #Plot means
  plot(means ~ x.values,
       xlim = x.limits,
       ylim = c(y.min,y.max),
       xaxt = "n",
       xlab = "",
       ylab = "",
       cex = 1.25,
       pch = 16)
  
  axis(side = 1, 
       at = x.values,
       labels = categories
  )
  
  #Plot upper error bar 
  lwd. <- 2
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.up,
         x1 = x.values,
         length = 0,
         lwd = lwd.)
  
  #Plot lower error bar
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.low,
         x1 = x.values,
         length = 0,
         lwd = lwd.) 
  
  #add reference line at 0
  abline(h = 0, col = 2, lwd = 2, lty =2)
  
  mtext(text = x.axis.label,side = 1,line = 1.75)
  mtext(text = y.axis.label,side = 2,line = 1.95)
  mtext(text = "Error bars = 95% CI",side = 3,line = 0,adj = 0)
  
  
}

#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####

t.fit <- TukeyHSD(fit, "factor(lan)", ordered = TRUE)
plotTukeysHSD(t.fit)

TukeyHSD(fit, "factor(kas)", ordered = TRUE)
TukeyHSD(fit, "factor(lan)", ordered = TRUE)

plot(TukeyHSD(fit, "factor(lan)", conf.level = 0.99))
plot(TukeyHSD(fit, "factor(kas)", conf.level = 0.99))


###
fitm1 <- lm(nitN ~  KuitLisKokN + KuitLisN + TOCN + TOCNliu+ LisC,
            data = Kuitub1)
# TOCN is best explaining the variation
# TOCN = Total organic carbon / total nitrogen in sludge

fitlm1 <- lm(nitN ~ kas*TOCN * KuitLisKokN,
             data = Kuitub1)
fitlm2 <- lm(nitN ~ kas*TOCN * KuitLisN,
             data = Kuitub1)
fitlm3 <- lm(formula = nitN ~ kas + TOCN + KuitLisKokN + kas:KuitLisKokN, data = Kuitub1)
# MASS::stepAIC(fitlm1)
# We can also explain variation without the lan, so we are now using the TOCN and added soluble nitrogen

AIC(fitlm1,fitlm2,fitlm3)

# model fitlm2 selected

summary(fitlm2)


#####################################################
# Plotting...
Kuitub1 = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "R" ), ]

raw_plot <- ggplot(Kuitub1, aes(x=lan,y=nitN))
raw_plot <- raw_plot + geom_point(position = position_jitter(w=0.2),alpha=0.4, colour="grey58")
raw_plot <- raw_plot + xlab("Treatment")
raw_plot <- raw_plot + ylab("Value")
raw_plot <- raw_plot #+ ylim(0,15)
raw_plot <- raw_plot + theme_tufte(base_size=18)+geom_rangeframe(color="black")
raw_plot

# Kuitub1$lan <- factor(Kuitub1$lan, levels=c("S1", "S2", "S3", "S4", "Min", "C"))
# To change the order, not needed now.
library(plyr)

summary_data <- ddply(Kuitub1,
                      c("lan"),
                      summarise,
                      N = length(nitN),
                      avg_value = mean(nitN),
                      sd = sd(nitN),
                      se = sd/sqrt(N)
)

# check out the summary stats 
summary_data

# we'll also do this for the summary data and use it later on 
# summary_data$treatment <- factor(summary_data$treatment, levels=("S1", "S2", "S3", "S4", "Min", "C"))

#Make labesl
summary_data$labels <- c("a","a","a","a","b","c")


combined_plot <- raw_plot + geom_point(data=summary_data, aes(x=lan,y=avg_value), colour="black", size=3)
combined_plot <- combined_plot + geom_errorbar(data = summary_data,
                                               aes(x = lan, y = avg_value,
                                                   ymax = avg_value + sd,ymin = avg_value - sd), 
                                               width=0.1, colour="black")
combined_plot <- combined_plot + annotate("text",x="S1",y=0,label="n=4")
combined_plot <- combined_plot + annotate("text",x="S2",y=0,label="n=4")
combined_plot <- combined_plot + annotate("text",x="S3",y=0,label="n=4")
combined_plot <- combined_plot + annotate("text",x="S4",y=0,label="n=4")
combined_plot <- combined_plot + annotate("text",x="Min",y=0,label="n=4")
combined_plot <- combined_plot + annotate("text",x="C",y=0,label="n=4")
combined_plot <- combined_plot + geom_text(data=summary_data,aes(x=lan,y=avg_value+se+3,label=labels))
combined_plot + ggtitle("R + nitrate levels @ 15.11.2015")

fit <- aov(nitN ~ factor(lan) + factor(ker),
           data = Kuitub1)
TukeyHSD(fit, "factor(lan)", conf.level = 0.95)
##################################################################################
#example
agg = ddply(Kuitub, .(kas, lan, time), function(x){
  c(mean=mean(x$nitN), sd = sd(x$nitN))
})

#agg #contains all nitN treatment means and standard deviations
#write.table(agg, "nitNmeans6times.txt", sep="\t")

agg$lower = agg$mean + agg$sd
agg$upper = agg$mean - agg$sd

#agg #contains mean, sd, upr, and lwr confidence levels

pd <- position_dodge(width = 0.2) # move them .2 to the left and right

#FIRST RUN ONE OF THESE

#Plot2x4
gbase  = ggplot(agg, aes(y=mean, colour=lan)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.3, position=pd) +
  geom_point(position=pd) + facet_wrap(~ kas, ncol=2)
gline = gbase + geom_line(position=pd) + ylim(0, 25)
print(gline + aes(x=time) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = "Time", y = "Nitrate NO3- (mg/kg) means with confidence intervals"))

agg$num_time = NA
agg$num_time = as.numeric(agg$time)

#for all graphs
gline = gline %+% agg
print(gline + aes(x=num_time) + scale_x_continuous(breaks= 1:length(agg$time), labels = agg$time)
      + theme(axis.text.x = element_text(angle=60, hjust=1))
      + labs(x = "Time", y = "Nitrate NO3- (mg/kg) means with confidence intervals"))

gline_final = gline + aes(x=num_time) + scale_x_continuous(breaks= 1:length(agg$time), labels = agg$time) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(x = "Time", y = "Nitrate NO3- (mg/kg) means with confidence intervals")

#Final graph
print(gline_final+theme_tufte()+geom_rangeframe(color="black"))

