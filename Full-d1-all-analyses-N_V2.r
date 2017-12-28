#Full analyses

#set the working directory & read the data
#On a Windows computer: adjust blah blah blah
setwd("c:/blah blah blah/Data/")
setwd("c:\\blah blah blah\\Data")

setwd("C:/HY-Data/TOPARVIA/OneDrive - University of Helsinki/Projektit/Kuitulietekoe2016/Analyysit")

#Temporal
setwd("C:/Users/HY-User.DO3-F28-K131-01.165/Documents/Work")

#For a Mac:
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data")


Kuitub <- read.table(file = "d1alln.txt",
                    header = TRUE,
                    dec = ".")

#Housekeeping
#Load packages from R and support functions that we wrote

#Needed
library(ggplot2) # for graphical presentations
library(lattice) # For fancy multipanel graphs
library(MASS) # for statistical calculations
library(lme4) # for mixed models
library(lmerTest) #for confidence intervals
library(arm) #for simulations



#Others
library(lattice)  #For fancy multipanel graphs
library(MASS) # for statistical calculations
#library(nlme) #old version of lme4
library(lme4)
source("HighstatLibV9.R") #Now we can make fancy graphs
library(ggplot2)

########################################################################
#Inspect the file
#What do we have?

names(Kuitub)

str(Kuitub)

#Naming the factor levels
#Turning to factors
Kuitub$lan=factor(Kuitub$lan)
Kuitub$ker=factor(Kuitub$ker)
Kuitub$time=factor(Kuitub$time)
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
levels = c(1,2,3,4,5),
labels = c("29.9.2015", "23.10.2015", "15.11.2015", "22.4.2016", "12.9.2016"))

#Treatments

p <- ggplot(Kuitub, aes(time, nitN))
p + geom_boxplot(aes(fill = lan)) + geom_jitter()
p + geom_boxplot() + geom_jitter()

#Subset by plant

KuitubP = Kuitub[ which(Kuitub$kas=='P'), ]
p <- ggplot(KuitubP, aes(time, nitN))
p + geom_boxplot(aes(fill = lan))


KuitubAK = Kuitub[ which(Kuitub$kas=='AK'), ]
p <- ggplot(KuitubAK, aes(time, nitN))
p + geom_boxplot(aes(fill = lan))


KuitubR = Kuitub[ which(Kuitub$kas=='R'), ]
p <- ggplot(KuitubR, aes(time, nitN))
p + geom_boxplot(aes(fill = lan))

KuitubKK = Kuitub[ which(Kuitub$kas=='KK'), ]
p <- ggplot(KuitubKK, aes(time, nitN))
p + geom_boxplot(aes(fill = lan))

#Subset by treatment

KuitubMin = Kuitub[ which(Kuitub$lan=='Min'), ]
p <- ggplot(KuitubMin, aes(time, nitN))
p + geom_boxplot(aes(fill = kas))


KuitubMin = Kuitub[ which(Kuitub$lan=='Min'), ]
p <- ggplot(KuitubMin, aes(time, nitN))
p + geom_jitter(aes(color = kas))


######################################################
#stat tests #nitN
######################################################
fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas) * factor(time),
			data = Kuitub)

model = fit

par(mfrow = c(2, 2))
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)

#######################################################
#Using multilevel models to get accurate inferences for repeated measures ANOVA designs
#######################################################
#https://www.r-bloggers.com/using-multilevel-models-to-get-accurate-inferences-for-repeated-measures-anova-designs/

install.packages('pbkrtest')

library(lme4)
library(pbkrtest)

fit.l <- lmer(nitN ~ factor(lan) + (1|ker) + (1|time), data=Kuitub)

fit.lk <- lmer(nitN ~ factor(lan) + factor(kas) + (1|ker) + (1|time), data=Kuitub)

KRmodcomp(fit.l, fit.lk)

#fit.mcmc <- mcmcsamp(fit.l, n = 2500) #2.5k samples
#fit.mcmc <- mcmcsamp(fit.l, n = 25000) #25k samples



#the mcmcsamp is no longer supported! --> bootMer is the way.
#Using bootMer to do model comparison in R
#https://www.r-bloggers.com/using-bootmer-to-do-model-comparison-in-r/
#######################################################
#Subset by time ANOVA
#######################################################
#Time: labels = c("29.9.2015", "23.10.2015", "15.11.2015", "22.4.2016", "12.9.2016"))
install.packages("agricolae")
install.packages("multcomp")
install.packages("multcompView")
install.packages("factorplot")
#Time: 29.9.2015 - 1

Kuitub1 = Kuitub[ which(Kuitub$time=='29.9.2015'), ]
p <- ggplot(Kuitub1, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
			data = Kuitub1)

fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
			data = Kuitub1)

#Tukey

tx <- with(Kuitub1, interaction(lan, kas))
amod <- aov(nitN ~ tx, data=Kuitub1)

tuk <- glht(amod, linfct = mcp(tx = "Tukey"))
tuk2 <- glht(amod, linfct = mcp(tx = "Tukey", interaction_average=TRUE))

summary(tuk)          # standard display
tuk.cld <- cld(tuk)   # letter-based display
opar <- par(mai=c(1,1,1.5,1)) #setting the margings for plot in inch
plot(tuk.cld)
par(opar)

#######################################################
#Time: 23.10.2015 - 2
Kuitub2 = Kuitub[ which(Kuitub$time=='23.10.2015'), ]
p <- ggplot(Kuitub2, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

t2fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
			data = Kuitub2)

t2fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
			data = Kuitub2)

#Tukey
tx <- with(Kuitub2, interaction(lan, kas))
amod <- aov(nitN ~ tx, data=Kuitub2)
tuk <- glht(amod, linfct = mcp(tx = "Tukey"))
tuk.cld <- cld(tuk)   # letter-based display


#Model validity
model = fit
model = fit2
model = t2fit
model = t2fit2

par(mfrow = c(2, 2))
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)

#######################################################
#Time: 15.11.2015 - 3
Kuitub3 = Kuitub[ which(Kuitub$time=='15.11.2015'), ]
p <- ggplot(Kuitub3, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

t3fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
			data = Kuitub3)

t3fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
			data = Kuitub3)

#is same
t3fit3 <- aov(nitN ~ factor(lan)*factor(kas),
			data = Kuitub3)

#Tukey, interaction
tx <- with(Kuitub3, interaction(lan, kas))
amod <- aov(nitN ~ tx, data=Kuitub3)
tuk <- glht(amod, linfct = mcp(tx = "Tukey"))
tuk.cld <- cld(tuk)   # letter-based display

#lan
TukeyHSD(t2fit2, "factor(lan)", ordered = TRUE)
plot(TukeyHSD(t5fit2, "factor(lan)"))
#kas
TukeyHSD(t2fit2, "factor(kas)", ordered = TRUE)
plot(TukeyHSD(t2fit2, "factor(kas)"))


#Model validity
model = fit
model = fit2
model = t2fit
model = t2fit2
model = t3fit2

par(mfrow = c(2, 2))
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)
#######################################################
#Time: 22.4.2016 - 4
Kuitub4 = Kuitub[ which(Kuitub$time=='22.4.2016'), ]
p <- ggplot(Kuitub4, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

t4fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
			data = Kuitub4)

t4fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
			data = Kuitub4)
t4fit3 <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker),
			data = Kuitub4)

anova(t4fit, t4fit2, t4fit3, test="Chisq")

#Tukey
tx <- with(Kuitub4, interaction(lan, kas))
amod <- aov(nitN ~ tx, data=Kuitub4)
tuk <- glht(amod, linfct = mcp(tx = "Tukey"))
tuk.cld <- cld(tuk)   # letter-based display


#Model validity
model = fit
model = fit2
model = t2fit
model = t2fit2
model = t3fit2
model = t4fit2


par(mfrow = c(2, 2))
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)
#######################################################
#Time: 12.9.2016 - 5
Kuitub5 = Kuitub[ which(Kuitub$time=='12.9.2016'), ]
p <- ggplot(Kuitub5, aes(kas, nitN))
p + geom_boxplot(aes(fill = lan))

t5fit <- aov(nitN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas),
			data = Kuitub5)

t5fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
			data = Kuitub5)

anova(t5fit, t5fit2, test="Chisq") #this is only for nested models
anova(t5fit, t5fit2, test="F")	#similar as drop1

drop1(t5fit)


#Tukey
tx <- with(Kuitub5, interaction(lan, kas))
amod <- aov(nitN ~ tx, data=Kuitub5)
tuk <- glht(amod, linfct = mcp(tx = "Tukey"))
tuk.cld <- cld(tuk)   # letter-based display

t5fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
			data = Kuitub5)
#lan
TukeyHSD(t5fit2, "factor(lan)", ordered = TRUE)
plot(TukeyHSD(t5fit2, "factor(lan)"))
#kas
TukeyHSD(t5fit2, "factor(kas)", ordered = TRUE)
plot(TukeyHSD(t5fit2, "factor(kas)"))

#Model validity
model = fit
model = fit2
model = t2fit
model = t2fit2
model = t3fit2
model = t4fit2
model = t5fit2


par(mfrow = c(2, 2))
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)


#######################################################
#TukeyHSD - different ways to get output
#######################################################
install.packages("agricolae")
install.packages("multcomp")
install.packages("multcompView")
install.packages("factorplot")

library("agricolae")
library("multcomp")
library("multcompView")
library("factorplot")
#vaihtoehtoiset tavat

#Multicomp
fit2 <- aov(nitN ~ factor(lan) + factor(kas) + factor(lan):factor(kas),
			data = Kuitub1)
# for reference

summary(test <- aov(nitN ~ lan + factor(kas) + factor(lan):factor(kas),
                    data = Kuitub1))
TukeyHSD(test, ordered = TRUE, conf.level = 0.95)

plot(TukeyHSD(test, "lan", ordered=TRUE))

summary(test2 <- aov(nitN ~ lan,
                    data = KuitubP))
TukeyHSD(test2, ordered = TRUE, conf.level = 0.95)

plot(TukeyHSD(test, "lan", ordered=TRUE))

# summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
# TukeyHSD(fm1, "tension", ordered = TRUE)
# plot(TukeyHSD(fm1, "tension"))




#######################################################
#http://glmm.wikidot.com/faq#modelspec

# Robin Jeffries, UCLA, grouping methods

#(1|group) 	random group intercept
#(x|group) = (1+x|group) 	random slope of x within group with correlated intercept
#(0+x|group) = (-1+x|group) 	random slope of x within group: no variation in intercept
#(1|group) + (0+x|group) 	uncorrelated random intercept and random slope within group
#(1|site/block) = (1|site)+(1|site:block) 	intercept varying among sites and among blocks within sites (nested random effects)
#site+(1|site:block) 	fixed effect of sites plus random variation in intercept among blocks within sites
#(x|site/block) = (x|site)+(x|site:block) = (1 + x|site)+(1+x|site:block) 	slope and intercept varying among sites and among blocks within sites
#(x1|site)+(x2|block) 	two different effects, varying at different levels
#x*site+(x|site:block) 	fixed effect variation of slope and intercept varying among sites and random variation of slope and intercept among blocks within sites
#(1|group1)+(1|group2) 	intercept varying among crossed random effects (e.g. site, year)




m <- lmer(nitN ~ lan + kas + (1 | time)
		+ (1 | ker),
		data = Kuitub
		)
m2 <- lmer(nitN ~ lan + kas + (1 | time),
		data = Kuitub
		)
m3 <- lmer(nitN ~ lan + kas + (1 | time/ker),
		data = Kuitub
		)
#m2 fit
par(mfrow=c(2,2))
plot(fitted(m2),resid(m3,type="pearson"),col="blue") #a plot to check the constant standard deviation
abline(h=0,lwd=2)
qqnorm(resid(m2)) #normality of the residuals
qqline(resid(m2))
qqnorm(ranef(m2)$time[,1]) #sample QQplot
qqline(ranef(m2)$time[,1]) #factor QQplot
#mtext(text="Pearson residuals, Sample QQ, Time factor QQ, Ker factor QQ ",side=3,outer=FALSE,at=-3)  #for text

###########################################################################
#bootstrapping
###########################################################################
sims<-sim(m2,n.sims=200)
str(sims)

#plotting the confidence intervals for estimates

crI<-apply(sims@fixef,2,function(x) quantile(x,probs=c(0.025,0.975))) #Bayesian probabilities to get 95% confidence intervals
plot(1:9,fixef(m2),ylim=c(-5,11),xaxt="n",xlim=c(0.5,10.5),xlab="") #check the number of factors in 1:9 and xlim to match
axis(1,at=1:9,labels=names(fixef(m2)))
segments(1:9,crI[1,],1:9,crI[2,])
abline(h=0,lty=2,lwd=2)
# are the factors different from zero?

#bootstrap Confidence Intervals
boots<-confint(m2,parm=5:7,method="boot",nsim=200)
points((1:9)-0.1,fixef(m2),col="red")
segments((1:9)-0.1,boots[,1],(1:9)-0.1,boots[,2],col="red")

#profile Confidence Intervals
prof<-confint(m2,parm=5:7,method="profile")
points((1:9)+0.1,fixef(m2),col="blue")
segments((1:9)+0.1,prof[,1],(1:9)+0.1,prof[,2],col="blue")
legend("topright",legend=c("Posterior sampling","Bootstrap","Profile"),lwd=1,col=c("black","red","blue"),bty="n")



#The first method use the sim function which randomly draw posterior samples of the coefficients based on the fitted models.
#We can draw as many sample as we want and based on Bayesian theory the 0.025 and 0.975 quantiles of the sampled values will
#form the 95% credible intervals around the fitted value (the one we got from the model output).
#The second method will simulate new Biomass data from the given explanatory variables and the fitted models, it will then re-fit the model to these
#simulated response values and will extract the fitted coefficient. From this distribution of bootstrapped coefficient it will draw a 95% confidence interval
#around the fitted values. Finally the third method will compute profile likelihood value and will determine when are these profile likelihood
#values not significantly different from the profile likelihood you would get if the parameter of interest was set to 0.
#This will also gives a 95% confidence interval. As always in R there are many ways to get what you want (one could also use the package lmerTest or afex)
#, based on your data you may choose which one to use.


#m3	fit
par(mfrow=c(2,2))
plot(fitted(m3),resid(m3,type="pearson"),col="blue") #a plot to check the constant standard deviation
abline(h=0,lwd=2)
qqnorm(resid(m3)) #normality of the residuals
qqline(resid(m3))
qqnorm(ranef(m3)$time[,1]) #sample QQplot
qqline(ranef(m3)$time[,1]) #factor QQplot
qqnorm(ranef(m3)$ker[,1]) #factor QQplot
qqline(ranef(m3)$ker[,1]) #factor QQplot
#mtext(text="Pearson residuals, Sample QQ, Time factor QQ, Ker factor QQ ",side=3,outer=FALSE,at=-3)  #for text

#ranef = A generic function to extract the conditional modes of the random effects from a fitted model object. For linear mixed models the conditional modes of the random effects are also the conditional means.

#example
par(mfrow=c(2,2))
plot(fitted(m1),resid(m1,type="pearson"),col="blue") #a plot to check the constant standard deviation
abline(h=0,lwd=2)
qqnorm(resid(m1)) #normality of the residuals
qqline(resid(m1))
qqnorm(ranef(m1)$Site[,1]) #factor QQplot
qqline(ranef(m1)$Site[,1]) #factor QQplot
qqnorm(ranef(m1)$Site[,2]) #factor QQplot
qqline(ranef(m1)$Site[,2]) #factor QQplot



########################################################

fit <- aov(liuN ~ factor(lan) + factor(kas) + factor(ker) + factor(lan):factor(kas) * factor(time),
			data = Kuitub)

model = fit

par(mfrow = c(2, 2))
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)


######################################################
#stat tests #nitN
######################################################


library(lmerTest)

m <- lmer(liuN ~ lan + kas + (1 | time)
		+ (1 | ker),
		data = Kuitub
		)
m2 <- lmer(liuN ~ lan + kas + (1 | time),
		data = Kuitub
		)

#Confidence interwals for m2

#Testing confint() merMod

#fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#fm1W <- confint(fm1, method="Wald")# very fast, but ....
#fm1W

testLevel <- if (nzchar(s <- Sys.getenv("LME4_TEST_LEVEL"))) as.numeric(s) else 1
if(interactive() || testLevel >= 3) {
 ## ~20 seconds, MacBook Pro laptop
 system.time(m2P <- confint(m2, method="profile", ## default
                             oldNames = FALSE))
 ## ~ 40 seconds
 system.time(m2B <- confint(m2,method="boot",
                             .progress="txt", PBargs=list(style=3)))
} else
  load(system.file("testdata","confint_ex.rda",package="lme4"))
m2P
m2B

m2w = confint(m2, method="Wald")


##################################################################################
#Plot Hell with Lines
##################################################################################
KuitubP = Kuitub[ which(Kuitub$kas=='P'), ]
p <- ggplot(KuitubP, aes(time, nitN))
p + geom_boxplot(aes(fill = lan))

library(plyr)
#example
agg = ddply(Kuitub, .(kas, lan, time), function(x){
  c(mean=mean(x$nitN), sd = sd(x$nitN))
})

agg #contains all nitN treatment means and standard deviations
write.table(agg, "nitNmeans5times.txt", sep="\t")

agg$lower = agg$mean + agg$sd
agg$upper = agg$mean - agg$sd

agg #contains mean, sd, upr, and lwr confidence levels

pd <- position_dodge(width = 0.2) # move them .2 to the left and right

#FIRST RUN ONE OF THESE

#Plot2x4
gbase  = ggplot(agg, aes(y=mean, colour=lan)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.3, position=pd) +
  geom_point(position=pd) + facet_wrap(~ kas, ncol=2)
gline = gbase + geom_line(position=pd) + ylim(0, 25)
print(gline + aes(x=time) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = "Time", y = "Nitrate NO3- (mg/kg) means with confidence intervals"))

#Plot4x6 lanxkas
gbase  = ggplot(agg, aes(y=mean, colour=lan)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.3, position=pd) +
  geom_point(position=pd) + facet_grid(lan ~ kas)
gline = gbase + geom_line(position=pd) + ylim(0, 25)
print(gline + aes(x=time) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = "Time", y = "Nitrate NO3- (mg/kg) means with confidence intervals"))

#Plot 3x2 lan
gbase  = ggplot(agg, aes(y=mean, colour=kas)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.3, position=pd) +
  geom_point(position=pd) + facet_wrap(~ lan, ncol=3)
gline = gbase + geom_line(position=pd) + ylim(0, 25)
print(gline + aes(x=time) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = "Time", y = "Nitrate NO3- (mg/kg) means with confidence intervals"))


#THEN RUN THE TIME CHANGE AND GET THE FINAL PLOT

#to get the lines, we need to change the factor to numeric
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

library(ggthemes)
#Final graph
print(gline_final+theme_tufte()+geom_rangeframe(color="black"))

#original
# gbase  = ggplot(agg, aes(y=mean, colour=lan)) +
#   geom_errorbar(aes(ymin=lower, ymax=upper), width=.3, position=pd) +
#   geom_point(position=pd) + facet_grid(variable ~ kas)
# gline = gbase + geom_line(position=pd)
# print(gline + aes(x=time))

