#Graphs
source("Data,packages,functions,versions.r")

# DATA...
#Kuitub1 = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "R" ), ]
#Kuitub12.9.2016R  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "R" ), ]
#Kuitub16.10.2017 = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "R" ), ]


MakeTukeyPlot <- function() {
summary_data <- ddply(Kuitub15.11.2015R,
                      c("lan"),
                      summarise,
                      N = length(nitN),
                      avg_value = mean(nitN),
                      sd = sd(nitN),
                      se = sd/sqrt(N)
)

# check out the summary stats 
summary_data


# Käsittelyjen keskiarvot, keskihajonnat sekä keskivirheet.


# Plotting
raw_plot <- ggplot(Kuitub15.11.2015R, aes(x=lan,y=nitN))
raw_plot <- raw_plot + geom_point(position = position_jitter(w=0.2),alpha=0.4, colour="grey58")
raw_plot <- raw_plot + xlab("Treatment")
raw_plot <- raw_plot + ylab("Value")
raw_plot <- raw_plot #+ ylim(0,15)
raw_plot <- raw_plot + theme_tufte(base_size=18)+geom_rangeframe(color="black")


# Kuitub1$lan <- factor(Kuitub1$lan, levels=c("S1", "S2", "S3", "S4", "Min", "C"))
# To change the order, not needed now.

# we'll also do this for the summary data and use it later on 
# summary_data$treatment <- factor(summary_data$treatment, levels=("S1", "S2", "S3", "S4", "Min", "C"))

#Make labels
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
combined_plot + ggtitle("R + nitraatti @ 15.11.2015")
}

# Käsittelyt S1-S4 ovat samoja, mineraalilannoite ja kontrolli eroavat toisistaan


fit <- aov(nitN ~ factor(lan) + factor(ker),
           data = Kuitub15.11.2015R)
TukeyHSD(fit, "factor(lan)", conf.level = 0.95)

summary_data <- ddply(Kuitub12.9.2016R,
                      c("lan"),
                      summarise,
                      N = length(nitN),
                      avg_value = mean(nitN),
                      sd = sd(nitN),
                      se = sd/sqrt(N)
)

# check out the summary stats 
summary_data


# Käsittelyjen keskiarvot, keskihajonnat sekä keskivirheet.


# Plotting
raw_plot <- ggplot(Kuitub12.9.2016R, aes(x=lan,y=nitN))
raw_plot <- raw_plot + geom_point(position = position_jitter(w=0.2),alpha=0.4, colour="grey58")
raw_plot <- raw_plot + xlab("Treatment")
raw_plot <- raw_plot + ylab("Value")
raw_plot <- raw_plot #+ ylim(0,15)
raw_plot <- raw_plot + theme_tufte(base_size=18)+geom_rangeframe(color="black")


# Kuitub1$lan <- factor(Kuitub1$lan, levels=c("S1", "S2", "S3", "S4", "Min", "C"))
# To change the order, not needed now.

# we'll also do this for the summary data and use it later on 
# summary_data$treatment <- factor(summary_data$treatment, levels=("S1", "S2", "S3", "S4", "Min", "C"))

#Make labels
summary_data$labels <- c("a","a","a","a","a","a")


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
combined_plot + ggtitle("R + nitraatti @ 12.9.2016")


# Käsittelyt S1-S4 ovat samoja, mineraalilannoite ja kontrolli eroavat toisistaan


fit <- aov(nitN ~ factor(lan) + factor(ker),
           data = Kuitub12.9.2016R)
TukeyHSD(fit, "factor(lan)", conf.level = 0.95)




# Data
# Kuitub16.10.2017
summary_data <- ddply(Kuitub16.10.2017,
                      c("lan"),
                      summarise,
                      N = length(nitN),
                      avg_value = mean(nitN),
                      sd = sd(nitN),
                      se = sd/sqrt(N)
)

# check out the summary stats 
summary_data

# Plotting
raw_plot <- ggplot(Kuitub16.10.2017, aes(x=lan,y=nitN))
raw_plot <- raw_plot + geom_point(position = position_jitter(w=0.2),alpha=0.4, colour="grey58")
raw_plot <- raw_plot + xlab("Treatment")
raw_plot <- raw_plot + ylab("Value")
raw_plot <- raw_plot #+ ylim(0,15)
raw_plot <- raw_plot + theme_tufte(base_size=18)+geom_rangeframe(color="black")


# Kuitub1$lan <- factor(Kuitub1$lan, levels=c("S1", "S2", "S3", "S4", "Min", "C"))
# To change the order, not needed now.

# we'll also do this for the summary data and use it later on 
# summary_data$treatment <- factor(summary_data$treatment, levels=("S1", "S2", "S3", "S4", "Min", "C"))

#Make labels
summary_data$labels <- c("a","a","a","a","a","a")


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
combined_plot + ggtitle("R + nitraatti @ Kuitub16.10.2017")

#Statistics
fit <- aov(nitN ~ factor(lan) + factor(ker),
           data = Kuitub16.10.2017)
TukeyHSD(fit, "factor(lan)", conf.level = 0.95)


## Sulfaattitulokset



# DATA...
Kuitub1 = Kuitub[ which(Kuitub$time=='15.11.2015'), ]
Kuitub16.10.2017 = Kuitub[ which(Kuitub$time=='12.9.2016'), ]

summary_data <- ddply(Kuitub1,
                      c("lan"),
                      summarise,
                      N = length(Smgkg),
                      avg_value = mean(Smgkg),
                      sd = sd(Smgkg),
                      se = sd/sqrt(N)
)

# check out the summary stats 
summary_data


# Käsittelyjen keskiarvot, keskihajonnat sekä keskivirheet.

# Plotting
raw_plot <- ggplot(Kuitub1, aes(x=lan,y=Smgkg))
raw_plot <- raw_plot + geom_point(position = position_jitter(w=0.2),alpha=0.4, colour="grey58")
raw_plot <- raw_plot + xlab("Treatment")
raw_plot <- raw_plot + ylab("Value")
raw_plot <- raw_plot #+ ylim(0,15)
raw_plot <- raw_plot + theme_tufte(base_size=18)+geom_rangeframe(color="black")


# Kuitub1$lan <- factor(Kuitub1$lan, levels=c("S1", "S2", "S3", "S4", "Min", "C"))
# To change the order, not needed now.

# we'll also do this for the summary data and use it later on 
# summary_data$treatment <- factor(summary_data$treatment, levels=("S1", "S2", "S3", "S4", "Min", "C"))

#Make labels
summary_data$labels <- c("a","a","b","a","b","b")


combined_plot <- raw_plot + geom_point(data=summary_data, aes(x=lan,y=avg_value), colour="black", size=3)
combined_plot <- combined_plot + geom_errorbar(data = summary_data,
                                               aes(x = lan, y = avg_value,
                                                   ymax = avg_value + sd,ymin = avg_value - sd), 
                                               width=0.1, colour="black")
combined_plot <- combined_plot + annotate("text",x="S1",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="S2",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="S3",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="S4",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="Min",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="C",y=0,label="n=16")
combined_plot <- combined_plot + geom_text(data=summary_data,aes(x=lan,y=avg_value+se+15,label=labels))
combined_plot + ggtitle("Kaikki kasvit + Sulfaatti @ 15.11.2015")


# Käsittelyt S1, S2 ja S4 ovat samoja, mineraalilannoite ja kontrolli sekä S3 eivät eroa toisistaan. Kolme neljästä lannoitteesta lisäsi rikkimäärää tilastollisesti merkitsevästi.

fit <- aov(Smgkg ~ factor(lan) + factor(ker),
           data = Kuitub1)
TukeyHSD(fit, "factor(lan)", conf.level = 0.95)


# DATA...
Kuitub1 = Kuitub[ which(Kuitub$time=='15.11.2015'), ]
Kuitub12.9.2016 = Kuitub[ which(Kuitub$time=='12.9.2016'), ]


summary_data <- ddply(Kuitub12.9.2016,
                      c("lan"),
                      summarise,
                      N = length(Smgkg),
                      avg_value = mean(Smgkg),
                      sd = sd(Smgkg),
                      se = sd/sqrt(N)
)

# check out the summary stats 
summary_data


# Käsittelyjen keskiarvot, keskihajonnat sekä keskivirheet.


# Plotting
raw_plot <- ggplot(Kuitub12.9.2016, aes(x=lan,y=Smgkg))
raw_plot <- raw_plot + geom_point(position = position_jitter(w=0.2),alpha=0.4, colour="grey58")
raw_plot <- raw_plot + xlab("Treatment")
raw_plot <- raw_plot + ylab("Value")
raw_plot <- raw_plot #+ ylim(0,15)
raw_plot <- raw_plot + theme_tufte(base_size=18)+geom_rangeframe(color="black")


# Kuitub1$lan <- factor(Kuitub1$lan, levels=c("S1", "S2", "S3", "S4", "Min", "C"))
# To change the order, not needed now.

# we'll also do this for the summary data and use it later on 
# summary_data$treatment <- factor(summary_data$treatment, levels=("S1", "S2", "S3", "S4", "Min", "C"))

#Make labels
summary_data$labels <- c("ab","ab","ab","a","ab","b")


combined_plot <- raw_plot + geom_point(data=summary_data, aes(x=lan,y=avg_value), colour="black", size=3)
combined_plot <- combined_plot + geom_errorbar(data = summary_data,
                                               aes(x = lan, y = avg_value,
                                                   ymax = avg_value + sd,ymin = avg_value - sd), 
                                               width=0.1, colour="black")
combined_plot <- combined_plot + annotate("text",x="S1",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="S2",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="S3",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="S4",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="Min",y=0,label="n=16")
combined_plot <- combined_plot + annotate("text",x="C",y=0,label="n=16")
combined_plot <- combined_plot + geom_text(data=summary_data,aes(x=lan,y=avg_value+se+15,label=labels))
combined_plot + ggtitle("Kaikki kasvit + Sulfaatti @ 12.9.2016")


# Käsittelyt S1-S3 ja mineraaali ovat samoja, S4 ja kontrolli eroavat toisistaan ja muista käsittelyistä. S4 eron selittää kaksi poikkeavaa havaintoa, jotka voivat olla sattumaa. Kasvikäsittelyillä ei ollut merkitsevää eroa, ja siksi käsittelyt on tässä yhdistetty. Käytännössä lannoitteiden rikkiä lisäävä vaikutus kestää kaksi satokautta.


fit <- aov(Smgkg ~ factor(lan) + factor(ker),
           data = Kuitub12.9.2016)
TukeyHSD(fit, "factor(lan)", conf.level = 0.95)

################################################################################

#Test data:
#  Kuitub15.11.2015R
#x = nitN
#y = lan


MakeTukeyPlotnitN <- function(data,title) {
  summary_data <- ddply(data,
                        c("lan"),
                        summarise,
                        N = length(nitN),
                        avg_value = mean(nitN),
                        sd = sd(nitN),
                        se = sd/sqrt(N)
  )
  
  # check out the summary stats 
  #summary_data
  
  
  # Käsittelyjen keskiarvot, keskihajonnat sekä keskivirheet.
  
  
  # Plotting
  raw_plot <- ggplot(data, aes(x=lan,y=nitN))
  raw_plot <- raw_plot + geom_point(position = position_jitter(w=0.2),alpha=0.4, colour="grey58")
  raw_plot <- raw_plot + xlab("")
  raw_plot <- raw_plot + ylab(expression(paste("Nitraatti ", NO[2*"-"]," (mg/kg)", sep="")))
  raw_plot <- raw_plot #+ ylim(0,15)
  raw_plot <- raw_plot + theme_tufte(base_size=18)+geom_rangeframe(color="black")
  
  
  # Kuitub1$lan <- factor(Kuitub1$lan, levels=c("S1", "S2", "S3", "S4", "Min", "C"))
  # To change the order, not needed now.
  
  # we'll also do this for the summary data and use it later on 
  # summary_data$treatment <- factor(summary_data$treatment, levels=("S1", "S2", "S3", "S4", "Min", "C"))
  
  #Make labels
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
  combined_plot + ggtitle(title)
}

MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015")
