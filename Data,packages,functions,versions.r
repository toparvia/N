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