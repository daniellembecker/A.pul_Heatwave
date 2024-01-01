#Title: Buoyant Weight Data
#Project: NSF BSF
#Author: HM Putnam 
#Edited by: HM Putnam
#Date Last Modified: 20180927
#See Readme file for details

#http://166.122.78.194:80/cgi-bin/datalog.xml?sdate=1806030000&days=2
#http://www.informit.com/articles/article.aspx?p=2215520

rm(list=ls()) #clears workspace 


library("XML")
library("plyr")
library("lubridate")

#############################################################
setwd("~/MyProjects/Holobiont_Integration/RAnalysis/Data") #set working directory
#############################################################
dat <- Sys.Date() #set todays date
#today <- gsub('-', '', dat) 
time <- Sys.time()
time <- gsub(":" ,"_",time)
time <- gsub(" " ,"_",time)
#HST <- force_tz(HST, format= %h:%m:%s, tzone = "Pacific/Honolulu")
today <- sub('..', '', dat)
today <- gsub('-', '', today) 
today <- as.numeric(today)-1
            
            
#Zoox1.xmlfile <- xmlParse(paste0("http://166.122.79.183:80/cgi-bin/datalog.xml?sdate=",today,"0000&days=2")) #read in the date (e.g. 180620) plus # days (e.g. days=4) of Apex data
Zoox1.xmlfile <- xmlParse("http://166.122.79.205/cgi-bin/datalog.xml?sdate=1811180000&days=2") #read in the date (e.g. 180620) plus # days (e.g. days=4) of Apex data
Zoox1.parsed <- ldply(xmlToList(Zoox1.xmlfile), data.frame) #convert xml to dataframe

write.csv(Zoox1.parsed, paste0("~/MyProjects/Holobiont_Integration/RAnalysis/Output/Apex_Output/",time,"_", "Apex_Data_Output.csv")) #write file to save data


Zoox<- Zoox1.parsed[4:nrow(Zoox1.parsed),] #remove extra metadata from top
Zoox <- head(Zoox,-2) #remove extra metadata from bottom
Zoox$date <- as.POSIXct(Zoox$date, format = "%m/%d/%Y %H:%M:%S", tz="HST") #convert date to HI time
Zoox1 <- Zoox  
Zoox1[] <- lapply(Zoox1[,4:length(Zoox1)], as.character) #change to character
colnames(Zoox1) <- Zoox1[1,] #assign names to columns to search for probes
params <- c("Tmp_1", "pH_1","Tmp_2", "pH_2","Tmp_3", "pH_3","Tmp_4", "pH_4","Tmp_5", "pH_5","Tmp_6", "pH_6","Tmp_7", "pH_7","Tmp_8", "pH_8","Tmp_9", "pH_9","Tmp_10", "pH_10","Tmp_11", "pH_11","Tmp_12", "pH_12", "PAR_1")  #rename columns

lst.name <- with(Zoox1, which(colnames(Zoox1) %in% params))
lst.num <- lst.name+2
lst.num <- lst.num[-length(lst.num)]
Zoox1.Probe.Data <- Zoox1[,c(lst.num)] #make a dataframe of the data
Zoox1.Probe.Data$Date.Time <- Zoox$date #add date.time


colnames(Zoox1.Probe.Data)[1] <- unique(Zoox1[lst.name[1]])
colnames(Zoox1.Probe.Data)[2] <- unique(Zoox1[lst.name[2]])
colnames(Zoox1.Probe.Data)[3] <- unique(Zoox1[lst.name[3]])
colnames(Zoox1.Probe.Data)[4] <- unique(Zoox1[lst.name[4]])
colnames(Zoox1.Probe.Data)[5] <- unique(Zoox1[lst.name[5]])
colnames(Zoox1.Probe.Data)[6] <- unique(Zoox1[lst.name[6]])
colnames(Zoox1.Probe.Data)[7] <- unique(Zoox1[lst.name[7]])
colnames(Zoox1.Probe.Data)[8] <- unique(Zoox1[lst.name[8]])
colnames(Zoox1.Probe.Data)[9] <- unique(Zoox1[lst.name[9]])
colnames(Zoox1.Probe.Data)[10] <- unique(Zoox1[lst.name[10]])
colnames(Zoox1.Probe.Data)[11] <- unique(Zoox1[lst.name[11]])
colnames(Zoox1.Probe.Data)[12] <- unique(Zoox1[lst.name[12]])
colnames(Zoox1.Probe.Data)[13] <- unique(Zoox1[lst.name[13]])
colnames(Zoox1.Probe.Data)[14] <- unique(Zoox1[lst.name[14]])
colnames(Zoox1.Probe.Data)[15] <- unique(Zoox1[lst.name[15]])
colnames(Zoox1.Probe.Data)[16] <- unique(Zoox1[lst.name[16]])
colnames(Zoox1.Probe.Data)[17] <- unique(Zoox1[lst.name[17]])
colnames(Zoox1.Probe.Data)[18] <- unique(Zoox1[lst.name[18]])
colnames(Zoox1.Probe.Data)[19] <- unique(Zoox1[lst.name[19]])
colnames(Zoox1.Probe.Data)[20] <- unique(Zoox1[lst.name[20]])
colnames(Zoox1.Probe.Data)[21] <- unique(Zoox1[lst.name[21]])
colnames(Zoox1.Probe.Data)[22] <- unique(Zoox1[lst.name[22]])
colnames(Zoox1.Probe.Data)[23] <- "PAR"
colnames(Zoox1.Probe.Data)[24] <- unique(Zoox1[lst.name[24]])
colnames(Zoox1.Probe.Data)[25] <- unique(Zoox1[lst.name[25]])
colnames(Zoox1.Probe.Data)[26] <- "Date.Time"

write.csv(Zoox1.Probe.Data, paste0("~/MyProjects/Holobiont_Integration/RAnalysis/Output/Apex_Output/",time,"_", "Apex_Data_Output.csv")) #write file to save data

#Probe.Data <- merge(Zoox1.Probe.Data, Zoox2.Probe.Data, by="Date.Time")
Probe.Data <- Zoox1.Probe.Data

#plot Temp and pH and save to output
pdf(paste0("~/MyProjects/Holobiont_Integration/RAnalysis/Output/Apex_Output/",time,"_Apex_Output.pdf"))
par(mfrow=c(2,1))
plot(as.numeric(as.character(Tmp_1)) ~ Date.Time, Probe.Data, col = "red", type="l", ylim=c(26, 31),  xlab="Time", ylab="Temperature °C")
lines(as.numeric(as.character(Tmp_2)) ~ Date.Time, Probe.Data, col = "orange")
lines(as.numeric(as.character(Tmp_3)) ~ Date.Time, Probe.Data, col = "yellow")
lines(as.numeric(as.character(Tmp_4)) ~ Date.Time, Probe.Data, col = "green")
lines(as.numeric(as.character(Tmp_5)) ~ Date.Time, Probe.Data, col = "blue")
lines(as.numeric(as.character(Tmp_6)) ~ Date.Time, Probe.Data, col = "cyan")
lines(as.numeric(as.character(Tmp_7)) ~ Date.Time, Probe.Data, col = "purple")
lines(as.numeric(as.character(Tmp_8)) ~ Date.Time, Probe.Data, col = "grey")
lines(as.numeric(as.character(Tmp_9)) ~ Date.Time, Probe.Data, col = "black")
lines(as.numeric(as.character(Tmp_10)) ~ Date.Time, Probe.Data, col = "brown")
lines(as.numeric(as.character(Tmp_11)) ~ Date.Time, Probe.Data, col = "pink")
lines(as.numeric(as.character(Tmp_12)) ~ Date.Time, Probe.Data, col = "darkgreen")
legend("bottomright", c("Tank_1", "Tank_2","Tank_3", "Tank_4","Tank_5", "Tank_6","Tank_7", "Tank_8","Tank_9", "Tank_10","Tank_11", "Tank_12"), col=c("red", "orange", "yellow","green","blue","cyan","purple","grey","black","brown","pink","darkgreen"),cex=0.4, lty=1,
       inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n")
axis.POSIXct(side=1, Probe.Data$Date.Time)

plot(as.numeric(as.character(pH_1)) ~ Date.Time, Probe.Data, col = "red", type="l", ylim=c(7.5, 8.1),  xlab="Time", ylab="pH NBS")
lines(as.numeric(as.character(pH_2)) ~ Date.Time, Probe.Data, col = "orange")
lines(as.numeric(as.character(pH_3)) ~ Date.Time, Probe.Data, col = "yellow")
lines(as.numeric(as.character(pH_4)) ~ Date.Time, Probe.Data, col = "green")
lines(as.numeric(as.character(pH_5)) ~ Date.Time, Probe.Data, col = "blue")
lines(as.numeric(as.character(pH_6)) ~ Date.Time, Probe.Data, col = "cyan")
lines(as.numeric(as.character(pH_7)) ~ Date.Time, Probe.Data, col = "purple")
lines(as.numeric(as.character(pH_8)) ~ Date.Time, Probe.Data, col = "grey")
lines(as.numeric(as.character(pH_9)) ~ Date.Time, Probe.Data, col = "black")
lines(as.numeric(as.character(pH_10)) ~ Date.Time, Probe.Data, col = "brown")
lines(as.numeric(as.character(pH_11)) ~ Date.Time, Probe.Data, col = "pink")
lines(as.numeric(as.character(pH_12)) ~ Date.Time, Probe.Data, col = "darkgreen")
axis.POSIXct(side=1, Probe.Data$Date.Time)
dev.off()

#plot Temp and pH and save to output
pdf(paste0("~/MyProjects/Holobiont_Integration/RAnalysis/Output/Apex_Output/",time,"_Apex_Light_Output.pdf"))
plot(as.numeric(as.character(PAR)) ~ Date.Time, Probe.Data, col = "Orange", type="l", ylim=c(0, 1000),  xlab="Time", ylab="Light µmol m-2 s-1")
dev.off()



# concatenate and sort data
Form1 <- read.csv("Apex_Hourly.csv", header=TRUE, sep=",", na.string="NA", as.is=TRUE)
Form1$Date.Time <-  format(as.Date(Form1$Date.Time, "%m/%d%y %H:%M"), "%Y-%m-%d %H:%M:%S")


A28 <- read.csv("../Output/Apex_Output/2018-09-28_00_01_34_Apex_Data_Output.csv", header=TRUE, sep=",", na.string="NA", as.is=TRUE)
A28 <- A28[seq(1, NROW(A28), by = 60),]

A29 <- read.csv("../Output/Apex_Output/2018-09-29_23_05_42_Apex_Data_Output.csv", header=TRUE, sep=",", na.string="NA", as.is=TRUE)
A29 <- A29[seq(1, NROW(A29), by = 60),]

Apex <- rbind(A28,A29)
Apex <- Apex[,-1]
Apex <- Apex[, c("Date.Time", "Tmp_1", "pH_1","Tmp_2", "pH_2","Tmp_3", "pH_3","Tmp_4", "pH_4",
                 "Tmp_5", "pH_5","Tmp_6", "pH_6","Tmp_7", "pH_7","Tmp_8", "pH_8",
                 "Tmp_9", "pH_9","Tmp_10", "pH_10","Tmp_11", "pH_11","Tmp_12", "pH_12","PAR")]


#Apex$Date.Time <- as.POSIXct(Apex$Date.Time , format = "%m/%d/%Y %H:%M:%S", tz="HST") #convert date to HI time


All.Data <- rbind(Form1,Apex)

##### All Apex Data #####

#All.Data <- read.csv("Apex_Hourly.csv", header=TRUE, sep=",", na.string="NA", as.is=TRUE)
All.Data$Date.Time <- as.POSIXct(All.Data$Date.Time, format = "%m/%d/%Y %H:%M", tz="HST") #convert date to HI time


#plot Temp and pH and save to output
pdf(paste0("~/MyProjects/Holobiont_Integration/RAnalysis/Output/Apex_Output/Hourly_Apex_Output.pdf"))
par(mfrow=c(2,1))
plot(as.numeric(as.character(Tmp_1)) ~ Date.Time, All.Data, col = "red", type="l", ylim=c(26, 31),  xlab="Time", ylab="Temperature °C")
lines(as.numeric(as.character(Tmp_2)) ~ Date.Time, All.Data, col = "orange")
lines(as.numeric(as.character(Tmp_3)) ~ Date.Time, All.Data, col = "yellow")
lines(as.numeric(as.character(Tmp_4)) ~ Date.Time, All.Data, col = "green")
lines(as.numeric(as.character(Tmp_5)) ~ Date.Time, All.Data, col = "blue")
lines(as.numeric(as.character(Tmp_6)) ~ Date.Time, All.Data, col = "cyan")
lines(as.numeric(as.character(Tmp_7)) ~ Date.Time, All.Data, col = "purple")
lines(as.numeric(as.character(Tmp_8)) ~ Date.Time, All.Data, col = "grey")
lines(as.numeric(as.character(Tmp_9)) ~ Date.Time, All.Data, col = "black")
lines(as.numeric(as.character(Tmp_10)) ~ Date.Time, All.Data, col = "brown")
lines(as.numeric(as.character(Tmp_11)) ~ Date.Time, All.Data, col = "pink")
lines(as.numeric(as.character(Tmp_12)) ~ Date.Time, All.Data, col = "darkgreen")
legend("bottomright", c("Tank_1", "Tank_2","Tank_3", "Tank_4","Tank_5", "Tank_6","Tank_7", "Tank_8","Tank_9", "Tank_10","Tank_11", "Tank_12"), col=c("red", "orange", "yellow","green","blue","cyan","purple","grey","black","brown","pink","darkgreen"),cex=0.4, lty=1,
       inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n")
axis.POSIXct(side=1, All.Data$Date.Time)

plot(as.numeric(as.character(pH_1)) ~ Date.Time, All.Data, col = "red", type="l", ylim=c(7.5, 8.1),  xlab="Time", ylab="pH NBS")
lines(as.numeric(as.character(pH_2)) ~ Date.Time, All.Data, col = "orange")
lines(as.numeric(as.character(pH_3)) ~ Date.Time, All.Data, col = "yellow")
lines(as.numeric(as.character(pH_4)) ~ Date.Time, All.Data, col = "green")
lines(as.numeric(as.character(pH_5)) ~ Date.Time, All.Data, col = "blue")
lines(as.numeric(as.character(pH_6)) ~ Date.Time, All.Data, col = "cyan")
lines(as.numeric(as.character(pH_7)) ~ Date.Time, All.Data, col = "purple")
lines(as.numeric(as.character(pH_8)) ~ Date.Time, All.Data, col = "grey")
lines(as.numeric(as.character(pH_9)) ~ Date.Time, All.Data, col = "black")
lines(as.numeric(as.character(pH_10)) ~ Date.Time, All.Data, col = "brown")
lines(as.numeric(as.character(pH_11)) ~ Date.Time, All.Data, col = "pink")
lines(as.numeric(as.character(pH_12)) ~ Date.Time, All.Data, col = "darkgreen")
axis.POSIXct(side=1, All.Data$Date.Time)
dev.off()

