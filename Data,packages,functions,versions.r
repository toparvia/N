require("easypackages") # for loading and installing many packages at one time
packages_to_load <- c("broom", "dplyr", "plyr", "tidyverse", "corrplot", "ggplot2", "GGally", "devtools", "ggthemes","multcompView")
packages(packages_to_load, prompt = FALSE) # lock'n'load install/load


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

##################################################################################################
# Plots which ever significance levels you want
MakeTukeyPlotnitNManual <- function(data,title,lettersTUKEY) {
  if(missing(lettersTUKEY)) {
    lettersTUKEY <- "noinput"
  }
  
  summary_data <- ddply(data,
                        c("lan"),
                        summarise,
                        N = length(nitN),
                        avg_value = mean(nitN),
                        sd = sd(nitN),
                        se = sd/sqrt(N)
  )
  # This function generates Tukey HSD plot with significanse levels
  # data refers to nitrate values, treatment levels
  # title is the plot title
  # lettersTUKEY is the singificance a vector with letter equal to number to treatment levels
  
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
  summary_data$labels <- lettersTUKEY
  if(identical(lettersTUKEY,"noinput")){
    summary_data$labels <- c("a","a","a","a","b","c")
    print("Input also significance levels")
  }
  
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

#example MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015", c("a","a","a","a","b","c"))

##################################################################################################
#Function that calculates the labels as well if the values are not input

MakeTukeyPlotnitN <- function(data,title,lettersTUKEY) {
  if(missing(lettersTUKEY)) {
    lettersTUKEY <- "noinput"
  }
  
  summary_data <- ddply(data,
                        c("lan"),
                        summarise,
                        N = length(nitN),
                        avg_value = mean(nitN),
                        sd = sd(nitN),
                        se = sd/sqrt(N)
                        
  )
  max_value = max(data$nitN) #this is place for the significance level
  
  # This function generates Tukey HSD plot with significanse levels
  # data refers to nitrate values, treatment levels
  # title is the plot title
  # lettersTUKEY is the singificance a vector with letter equal to number to treatment levels
  # If lettersTUKEY is missing, it is automatically calculated using TUKEY HSD and multicompView
  
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
  summary_data$labels <- lettersTUKEY
  if(identical(lettersTUKEY,"noinput")){
    
    Tukey.levels <- aov(nitN ~ factor(lan) + factor(ker),
                        data = data)
    
    Tukey.levels <- TukeyHSD(Tukey.levels,   "factor(lan)", conf.level = 0.95)
    
    variable <- "factor(lan)"
    Tukey.levels <- Tukey.levels[[variable]][,4]
    Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
    
    #I need to put the labels in the same order :
    Tukey.labels$variable=rownames(Tukey.labels)
    labelslan = c("S1", "S2", "S3","S4", "Min", "C")
    Tukey.labels=Tukey.labels[order(match(Tukey.labels$variable,labelslan)),]
    
    #Just for error solving
    #print(Tukey.labels[1])
    #print(Tukey.levels)
    #print(summary(Tukey.levels))
    #summary_data$labels <- Tukey.labels[1]
    #dummy <- c("a","a","a","a","b","c")
    #print("Tukey HSD letters calculated automatically from the data. You can input your own giving vector of the letters")
    summary_data$labels <- Tukey.labels[1]
  }
  
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
  combined_plot <- combined_plot + geom_text(data=summary_data,aes(x=lan,y=max_value, label=labels))
  combined_plot + ggtitle(title)
  # + geom_text(data=summary_data,aes(x=lan,y=avg_value+se+3,label=labels)) old
  }


# example to show how ordering part of the function is build
# First solution, gives C, Min, S1 order...
# TKuitub29.9.2015$`factor(lan)`[order(TKuitub29.9.2015[["factor(lan)"]][,4],decreasing = FALSE)]
# Second Solution
# labelslan = c("S1", "S2", "S3","S4", "Min", "C")
# Label29.9.2015[order(match(Label29.9.2015$variable,labelslan)),]


# TESTED ON:
# R version 3.4.3 (2017-11-30)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Finnish_Finland.1252  LC_CTYPE=Finnish_Finland.1252   
# [3] LC_MONETARY=Finnish_Finland.1252 LC_NUMERIC=C                    
# [5] LC_TIME=Finnish_Finland.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] multcompView_0.1-7 ggthemes_3.4.0     devtools_1.13.5    GGally_1.3.2      
# [5] corrplot_0.84      forcats_0.3.0      stringr_1.3.0      purrr_0.2.4       
# [9] readr_1.1.1        tidyr_0.8.0        tibble_1.4.2       ggplot2_2.2.1     
# [13] tidyverse_1.2.1    plyr_1.8.4         dplyr_0.7.4        broom_0.4.3       
# [17] easypackages_0.1.0
# 
# loaded via a namespace (and not attached):
#   [1] reshape2_1.4.3     haven_1.1.1        lattice_0.20-35    colorspace_1.3-2  
# [5] htmltools_0.3.6    yaml_2.1.17        rlang_0.2.0        pillar_1.2.1      
# [9] foreign_0.8-69     glue_1.2.0         withr_2.1.1        RColorBrewer_1.1-2
# [13] modelr_0.1.1       readxl_1.0.0       bindrcpp_0.2       bindr_0.1         
# [17] munsell_0.4.3      gtable_0.2.0       cellranger_1.1.0   rvest_0.3.2       
# [21] psych_1.7.8        memoise_1.1.0      evaluate_0.10.1    knitr_1.20        
# [25] parallel_3.4.3     Rcpp_0.12.15       backports_1.1.2    scales_0.5.0      
# [29] jsonlite_1.5       mnormt_1.5-5       hms_0.4.1          digest_0.6.15     
# [33] stringi_1.1.6      grid_3.4.3         rprojroot_1.3-2    cli_1.0.0         
# [37] tools_3.4.3        magrittr_1.5       lazyeval_0.2.1     crayon_1.3.4      
# [41] pkgconfig_2.0.1    xml2_1.2.0         lubridate_1.7.2    reshape_0.8.7     
# [45] assertthat_0.2.0   rmarkdown_1.9      httr_1.3.1         rstudioapi_0.7    
# [49] R6_2.2.2           nlme_3.1-131       compiler_3.4.3