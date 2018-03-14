#Graphs
source("Data,packages,functions,versions.r")

# DATA...
#Kuitub1 = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "R" ), ]
#Kuitub12.9.2016R  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "R" ), ]
#Kuitub16.10.2017 = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "R" ), ]



#MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015", c("a","a","a","a","b","c"))

#Write the plotting commands for the functions

MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub23.10.2015R, "Ruis, nitraatti @ 23.10.2015")
MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub22.4.2016R,  "Ruis, nitraatti @ 22.4.2016")
MakeTukeyPlotnitN(Kuitub12.9.2016R,  "Ruis, nitraatti @ 12.9.2016")
MakeTukeyPlotnitN(Kuitub16.10.2017R, "Ruis, nitraatti @ 16.10.2017")
