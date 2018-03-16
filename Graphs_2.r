#Graphs
source("Data,packages,functions,versions.r")

# DATA...
#Kuitub1 = Kuitub[ which(Kuitub$time=='15.11.2015' & Kuitub$kas == "R" ), ]
#Kuitub12.9.2016R  = Kuitub[ which(Kuitub$time=='12.9.2016'  & Kuitub$kas == "R" ), ]
#Kuitub16.10.2017 = Kuitub[ which(Kuitub$time=='16.10.2017' & Kuitub$kas == "R" ), ]



#MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015", c("a","a","a","a","b","c"))

#Write the plotting commands for the functions
# c("R", "KK", "P","AK")

somePDFPath = "Results//results_nitrate.pdf"
pdf(file=somePDFPath) 

MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub23.10.2015R, "Ruis, nitraatti @ 23.10.2015")
MakeTukeyPlotnitN(Kuitub15.11.2015R, "Ruis, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub22.4.2016R,  "Ruis, nitraatti @ 22.4.2016")
MakeTukeyPlotnitN(Kuitub12.9.2016R,  "Ruis, nitraatti @ 12.9.2016")
MakeTukeyPlotnitN(Kuitub16.10.2017R, "Ruis, nitraatti @ 16.10.2017")

MakeTukeyPlotnitN(Kuitub15.11.2015KK, "Kerääjäkasvi, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub23.10.2015KK, "Kerääjäkasvi, nitraatti @ 23.10.2015")
MakeTukeyPlotnitN(Kuitub15.11.2015KK, "Kerääjäkasvi, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub22.4.2016KK,  "Kerääjäkasvi, nitraatti @ 22.4.2016")
MakeTukeyPlotnitN(Kuitub12.9.2016KK,  "Kerääjäkasvi, nitraatti @ 12.9.2016")
MakeTukeyPlotnitN(Kuitub16.10.2017KK, "Kerääjäkasvi, nitraatti @ 16.10.2017")

MakeTukeyPlotnitN(Kuitub15.11.2015P, "Perus, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub23.10.2015P, "Perus, nitraatti @ 23.10.2015")
MakeTukeyPlotnitN(Kuitub15.11.2015P, "Perus, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub22.4.2016P,  "Perus, nitraatti @ 22.4.2016")
MakeTukeyPlotnitN(Kuitub12.9.2016P,  "Perus, nitraatti @ 12.9.2016")
MakeTukeyPlotnitN(Kuitub16.10.2017P, "Perus, nitraatti @ 16.10.2017")

MakeTukeyPlotnitN(Kuitub15.11.2015AK, "Aluskasvi, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub23.10.2015AK, "Aluskasvi, nitraatti @ 23.10.2015")
MakeTukeyPlotnitN(Kuitub15.11.2015AK, "Aluskasvi, nitraatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub22.4.2016AK,  "Aluskasvi, nitraatti @ 22.4.2016")
MakeTukeyPlotnitN(Kuitub12.9.2016AK,  "Aluskasvi, nitraatti @ 12.9.2016")
MakeTukeyPlotnitN(Kuitub16.10.2017AK, "Aluskasvi, nitraatti @ 16.10.2017")

dev.off() 

somePDFPath = "Results//results_sulphate.pdf"
pdf(file=somePDFPath) 

MakeTukeyPlotSmgkg(Kuitub15.11.2015R, "Ruis, sulfaatti @ 15.11.2015")
MakeTukeyPlotSmgkg(Kuitub23.10.2015R, "Ruis, sulfaatti @ 23.10.2015")
MakeTukeyPlotSmgkg(Kuitub15.11.2015R, "Ruis, sulfaatti @ 15.11.2015")
MakeTukeyPlotSmgkg(Kuitub22.4.2016R,  "Ruis, sulfaatti @ 22.4.2016")
MakeTukeyPlotSmgkg(Kuitub12.9.2016R,  "Ruis, sulfaatti @ 12.9.2016")
MakeTukeyPlotSmgkg(Kuitub16.10.2017R, "Ruis, sulfaatti @ 16.10.2017")

MakeTukeyPlotSmgkg(Kuitub15.11.2015KK, "Kerääjäkasvi, sulfaatti @ 15.11.2015")
MakeTukeyPlotSmgkg(Kuitub23.10.2015KK, "Kerääjäkasvi, sulfaatti @ 23.10.2015")
MakeTukeyPlotSmgkg(Kuitub15.11.2015KK, "Kerääjäkasvi, sulfaatti @ 15.11.2015")
MakeTukeyPlotSmgkg(Kuitub22.4.2016KK,  "Kerääjäkasvi, sulfaatti @ 22.4.2016")
MakeTukeyPlotSmgkg(Kuitub12.9.2016KK,  "Kerääjäkasvi, sulfaatti @ 12.9.2016")
MakeTukeyPlotSmgkg(Kuitub16.10.2017KK, "Kerääjäkasvi, sulfaatti @ 16.10.2017")

MakeTukeyPlotSmgkg(Kuitub15.11.2015P, "Perus, sulfaatti @ 15.11.2015")
MakeTukeyPlotSmgkg(Kuitub23.10.2015P, "Perus, sulfaatti @ 23.10.2015")
MakeTukeyPlotSmgkg(Kuitub15.11.2015P, "Perus, sulfaatti @ 15.11.2015")
MakeTukeyPlotSmgkg(Kuitub22.4.2016P,  "Perus, sulfaatti @ 22.4.2016")
MakeTukeyPlotSmgkg(Kuitub12.9.2016P,  "Perus, sulfaatti @ 12.9.2016")
MakeTukeyPlotSmgkg(Kuitub16.10.2017P, "Perus, sulfaatti @ 16.10.2017")

MakeTukeyPlotnitN(Kuitub15.11.2015AK, "Aluskasvi, sulfaatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub23.10.2015AK, "Aluskasvi, sulfaatti @ 23.10.2015")
MakeTukeyPlotnitN(Kuitub15.11.2015AK, "Aluskasvi, sulfaatti @ 15.11.2015")
MakeTukeyPlotnitN(Kuitub22.4.2016AK,  "Aluskasvi, sulfaatti @ 22.4.2016")
MakeTukeyPlotnitN(Kuitub12.9.2016AK,  "Aluskasvi, sulfaatti @ 12.9.2016")
MakeTukeyPlotnitN(Kuitub16.10.2017AK, "Aluskasvi, sulfaatti @ 16.10.2017")

dev.off() 