
#In order to replicate results, all directory commands will need to be altered

rm(list=ls())
setwd('E:\\Dropbox\\CarnegieMellon\\Research\\Project4_CAFEavis\\Analysis_Writeup')

#####################################################################################################
#Load packages
library(symbols)
library(data.table)
library(calibrate)
library(SDMTools)
library(Hmisc)
library(directlabels)
library(lattice)
library(akima)


#####################################################################################################

#####################################################################################################
#Figure on historical CAFE standards
standards <- read.csv('inputs/standards.csv')
mpgscale <- data.frame('mpg'=seq(15,100,by=5))
mpgscale$gCO2.per.gal <- 8889/mpgscale$mpg
oldstandards <- read.csv('inputs/oldstandards.csv')

#convert old standards to grams CO2 per mile
oldstandards[,2:6] <- (8.92*10^3)/oldstandards[,2:6]

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figure1.pdf',height=8,width=12)
par(mar=c(5,5,4,5)+0.1)
plot(c(oldstandards$year,standards$year),c(oldstandards$passenger.cafe,standards$passengercars),type='n',xlab="Year",ylab=expression('Grams of CO'[2]~'/mile'),ylim=c(0,600),cex.axis=1.3,cex.lab=1.5)
abline(v=c(1978,1979,1986,1990,1997,2005,2012,2016),col="grey",lwd=1.2)
lines(c(oldstandards$year,standards$year),c(oldstandards$passenger.cafe,standards$passengercars),col="blue",lwd=2)
lines(c(oldstandards$year[2:34],standards$year),c(oldstandards$truck.cafe[2:34],standards$trucks),col="red",lwd=2)
lines(standards$year,standards$combined,col="forestgreen",lwd=2)
lines(oldstandards$year,oldstandards$passenger.actual,col="blue",lwd=2,lty=2)
lines(oldstandards$year[-1],oldstandards$truck.actual[-1],col="red",lwd=2,lty=2)
lines(oldstandards$year,oldstandards$combine.actual,col="forestgreen",lwd=2,lty=2)
text(x=c(1978,1979,1986,1990,1997,2005,2012,2016,1990,1980),y=c(600,600,600,600,600,600,600,600,400,565),pos=c(4,4,4,4,4,4,4,4,4,4,4),labels=c("1","2","3","4","5","6","7,8","9","Actual","CAFE Standards"),cex=1.3)
text(x=2016.7,y=215,pos=1,labels='Passenger Cars',srt=-22,cex=1.4)
text(x=2017,y=250,pos=4,labels='Combined',srt=-19,cex=1.4)
text(x=2014,y=330,pos=4,labels='Light-duty Trucks',srt=-19,cex=1.4)
axis(4,at=mpgscale$gCO2.per.gal,labels=mpgscale$mpg,cex.axis=1.3)
mtext("Miles per Gallon",side=4,line=3,padj=0,cex=1.5)
legend("bottomleft",c("1: Standards set for passenger vehicles (1978)","2: Standards set for light-duty trucks (1979)","3: NHTSA relaxes car standards (1986)", "4: NHTSA sets car standards to 27.5 MPG (1990)","5: Congress sets truck standards to 20.7 MPG (1997)","6: Bush Administration sets new truck standards (2005)","7: CAFE changed to footprint-based (2012)","8: NHTSA/EPA new standards, AFV incentives introduced (2012-2016)","9: NHTSA/EPA new standards (2017-2025)"),cex=1.25,bg="white")
dev.off()
#####################################################################################################

#####################################################################################################
#Footprint standards
car.footprints <- read.csv('inputs/car_footprints.csv',header=TRUE)
truck.footprints <- read.csv('inputs/truck_footprints.csv',header=TRUE)

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi1.pdf',height=6,width=12)
par(mar=c(5,5,2,5),mfrow=c(1,2))
plot(car.footprints$footprint,car.footprints$X2012,type="n",ylim=c(0,max(truck.footprints[,2:14])),xlab="Footprint of Vehicle (sq ft)",ylab=expression("Emissions Rate (g CO"[2]~"/mi)"),cex.lab=1.5,xaxt='n',yaxt='n',main='Passenger Cars')
axis(1,las=0,cex.axis=1.5)
axis(2,las=2,cex.axis=1.5)
axis(4,at=mpgscale$gCO2.per.gal,labels=mpgscale$mpg,cex.axis=1.5)
mtext("Miles per Gallon",side=4,line=3,padj=0,cex=1.5)
for(i in 2:length(car.footprints)) {
	lines(car.footprints$footprint,car.footprints[,i],col="blue",lwd=2)
}
text(x=c(50,50),y=c(340,140),pos=c(1,1),labels=c("2012","2025"),cex=1.5)
arrows(50,315,50,140,length=.1,lwd=2)
plot(truck.footprints$footprint,truck.footprints$X2012,type="n",ylim=c(0,max(truck.footprints[,2:14])),xlab="Footprint of Vehicle (sq ft)",ylab="Emissions Rate (g CO2/mi)",cex.lab=1.5,xaxt='n',yaxt='n',main='Light-duty Trucks')
axis(1,las=0,cex.axis=1.5)
axis(2,las=2,cex.axis=1.5)
axis(4,at=mpgscale$gCO2.per.gal,labels=mpgscale$mpg,cex.axis=1.5)
mtext("Miles per Gallon",side=4,line=3,padj=0,cex=1.5)
for(i in 2:length(car.footprints)) {
	lines(truck.footprints$footprint,truck.footprints[,i],col="red",lwd=2)
}
text(x=c(50,50),y=c(370,175),pos=c(1,1),labels=c("2012","2025"),cex=1.5)
arrows(50,345,50,175,length=.1,lwd=2)
dev.off()

#####################################################################################################

#####################################################################################################
#Evaluating current compliance
compliance.2009 <- read.csv('inputs/2009_compliance.csv',header=TRUE)
compliance.2009.cars <- compliance.2009[compliance.2009$Type=='car',]
compliance.2009.trucks <- compliance.2009[compliance.2009$Type=='truck',]

manuAmerican <- c('CHR','FMC','GMC','TSL')
manuEuropean <- c('AML','BMW','DAM','FIA','JAG','LOT','MAS','POR','SPK','VWA','VOL')
manuAsian <- c('FUJ','HON','HYU','KIA','MAZ','MIT','NIS','TOY','SUZ')

#Compliance by manufacturer
#For cars
compliance.2009.cars$weighted.em <- compliance.2009.cars$EmissionsRate*compliance.2009.cars$FinalSales
compliance.2009.cars$weighted.fpt <- compliance.2009.cars$FPT*compliance.2009.cars$FinalSales
dt.comp.cars <- data.table(compliance.2009.cars[,c(-2,-6)])
dt.comp.sum.cars <- dt.comp.cars[,lapply(.SD,sum),by=MFR]
dt.comp.sum.cars$final.em <- dt.comp.sum.cars$weighted.em/dt.comp.sum.cars$FinalSales
dt.comp.sum.cars$final.fpt <- dt.comp.sum.cars$weighted.fpt/dt.comp.sum.cars$FinalSales
dt.comp.sum.cars$country <- NA
for(row in 1:nrow(dt.comp.sum.cars)){
	if(dt.comp.sum.cars$MFR[row]%in%manuAmerican){dt.comp.sum.cars$country[row]<-'Black'}
	else if(dt.comp.sum.cars$MFR[row]%in%manuAsian){dt.comp.sum.cars$country[row]<-'Red'}
	else if(dt.comp.sum.cars$MFR[row]%in%manuEuropean){dt.comp.sum.cars$country[row]<-'Blue'}
}

#For trucks
compliance.2009.trucks$weighted.em <- compliance.2009.trucks$EmissionsRate*compliance.2009.trucks$FinalSales
compliance.2009.trucks$weighted.fpt <- compliance.2009.trucks$FPT*compliance.2009.trucks$FinalSales
dt.comp.trucks <- data.table(compliance.2009.trucks[,c(-2,-6)])
dt.comp.sum.trucks <- dt.comp.trucks[,lapply(.SD,sum),by=MFR]
dt.comp.sum.trucks$final.em <- dt.comp.sum.trucks$weighted.em/dt.comp.sum.trucks$FinalSales
dt.comp.sum.trucks$final.fpt <- dt.comp.sum.trucks$weighted.fpt/dt.comp.sum.trucks$FinalSales
dt.comp.sum.trucks$country <- NA
for(row in 1:nrow(dt.comp.sum.trucks)){
	if(dt.comp.sum.trucks$MFR[row]%in%manuAmerican){dt.comp.sum.trucks$country[row]<-'Black'}
	else if(dt.comp.sum.trucks$MFR[row]%in%manuAsian){dt.comp.sum.trucks$country[row]<-'Red'}
	else if(dt.comp.sum.trucks$MFR[row]%in%manuEuropean){dt.comp.sum.trucks$country[row]<-'Blue'}
}

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi2.pdf',height=6,width=12)
par(mar=c(5,5,2,5),mfrow=c(1,2))
plot(dt.comp.sum.cars$final.fpt,dt.comp.sum.cars$final.em,xlab="Vehicle Footprint (sq ft)",ylab=expression("Emissions Rate (g CO"[2]~"/mi)"),xaxt='n',yaxt='n',type='n',cex.lab=1.5,ylim=c(0,max(dt.comp.sum.cars$final.em)),main='Passenger Cars')
axis(1,las=0,cex.axis=1.5)
axis(2,las=2,cex.axis=1.5)
axis(4,at=mpgscale$gCO2.per.gal,labels=mpgscale$mpg,cex.axis=1.5)
mtext("Fuel Efficiency (mi/gal)",side=4,line=3,padj=0,cex=1.5)
points(x=dt.comp.sum.cars$final.fpt,y=dt.comp.sum.cars$final.em,pch=20,cex=1.2,col=dt.comp.sum.cars$country)
abline(h=8889/27.5)
text(x=43,y=340,pos=4,labels='2009 Standard',srt=0,cex=1.2,font=2)
lines(car.footprints$footprint,car.footprints$X2012,lty=2)
text(x=45,y=150,pos=4,labels='2012 Standard',srt=0,cex=1.2,font=2)
arrows(x0=45.2,y0=153,x1=44,y1=253,lwd=2,len=.1)
lines(car.footprints$footprint,car.footprints$X2016,lty=2)
text(x=46,y=100,pos=4,labels='2016 Standard',srt=0,cex=1.2,font=2)
arrows(x0=50.2,y0=105,x1=51,y1=245,lwd=2,len=.1)
text(x=dt.comp.sum.cars$final.fpt,y=dt.comp.sum.cars$final.em,labels=dt.comp.sum.cars$MFR,pos=c(2,4,3,3,2,3,2,1,1,2,2,2,2,1,4,3,4,3,3,1,3,2),cex=1.1)
legend('topright',c('American','Asian','European'),pch=c(20,20,20),col=c('Black','Red','Blue'))
plot(dt.comp.sum.trucks$final.fpt,dt.comp.sum.trucks$final.em,xlab="Vehicle Footprint (sq ft)",ylab=expression("Emissions Rate (g CO"[2]~"/mi)"),xaxt='n',yaxt='n',ylim=c(0,max(dt.comp.sum.cars$final.em)),type='n',cex.lab=1.5,main='Light-duty Trucks')
axis(1,las=0,cex.axis=1.5)
axis(2,las=2,cex.axis=1.5)
axis(4,at=mpgscale$gCO2.per.gal,labels=mpgscale$mpg,cex.axis=1.5)
#axis(4,at=mpgscale$gCO2.per.gal,labels=mpgscale$mpg,cex.axis=1.5)
mtext("Fuel Efficiency (mi/gal)",side=4,line=3,padj=0,cex=1.5)
points(x=dt.comp.sum.trucks$final.fpt,y=dt.comp.sum.trucks$final.em,pch=20,cex=1.2,col=dt.comp.sum.trucks$country)
abline(h=8889/23.1)
text(x=48,y=395,pos=4,labels='2009 Standard',srt=0,cex=1.2,font=2)
lines(truck.footprints$footprint,truck.footprints$X2012,lty=2)
text(x=48,y=210,pos=4,labels='2012 Standard',srt=0,cex=1.2,font=2)
arrows(x0=49,y0=225,x1=49,y1=320,lwd=2,len=.1)
lines(truck.footprints$footprint,truck.footprints$X2016,lty=2)
text(x=50,y=160,pos=4,labels='2016 Standard',srt=0,cex=1.2,font=2)
arrows(x0=53,y0=180,x21=53,y1=285,lwd=2,len=.1)
text(x=dt.comp.sum.trucks$final.fpt,y=dt.comp.sum.trucks$final.em,labels=dt.comp.sum.trucks$MFR,pos=c(3,1,3,2,1,2,2,1,3,1,1,3,1,4,3,2),cex=1.1)
legend('topright',c('American','Asian','European'),pch=c(20,20,20),col=c('Black','Red','Blue'))
dev.off()


#Evaluating compliance in 2012
complianceCars2012 <- read.csv('inputs/2012complianceCars.csv',header=TRUE)
for(row in 1:nrow(complianceCars2012)){
	if(complianceCars2012$manufacturer[row]%in%manuAmerican){complianceCars2012$country[row]<-'Black'}
	else if(complianceCars2012$manufacturer[row]%in%manuAsian){complianceCars2012$country[row]<-'Red'}
	else if(complianceCars2012$manufacturer[row]%in%manuEuropean){complianceCars2012$country[row]<-'Blue'}
}
complianceTrucks2012 <- read.csv('inputs/2012complianceTrucks.csv',header=TRUE)
for(row in 1:nrow(complianceTrucks2012)){
	if(complianceTrucks2012$manufacturer[row]%in%manuAmerican){complianceTrucks2012$country[row]<-'Black'}
	else if(complianceTrucks2012$manufacturer[row]%in%manuAsian){complianceTrucks2012$country[row]<-'Red'}
	else if(complianceTrucks2012$manufacturer[row]%in%manuEuropean){complianceTrucks2012$country[row]<-'Blue'}
}

carFootprintCalculator <- function(targetInput) {
	(targetInput-50.5)/4.72
}
complianceCars2012$footprint <- sapply(complianceCars2012$targetRate,carFootprintCalculator)
complianceCars2012$labPosX <- complianceCars2012$footprint
complianceCars2012$labPosX[complianceCars2012$manufacturer=='FMC'] <- complianceCars2012$labPosX[complianceCars2012$manufacturer=='FMC']-.7
complianceCars2012$labPosX[complianceCars2012$manufacturer=='NIS'] <- complianceCars2012$labPosX[complianceCars2012$manufacturer=='NIS']+.7
complianceCars2012$labPosX[complianceCars2012$manufacturer=='FUJ'] <- complianceCars2012$labPos[complianceCars2012$manufacturer=='FUJ']-.3

truckFootprintCalculator <- function(targetInput) {
	(targetInput-128.6)/4.04
}
complianceTrucks2012$footprint <- sapply(complianceTrucks2012$targetRate,truckFootprintCalculator)
complianceTrucks2012$labPosX <- complianceTrucks2012$footprint
complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='JAG'] <- complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='JAG']+1
complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='HON'] <- complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='HON']-.5
complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='VWA'] <- complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='VWA']+.5
complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='DAM'] <- complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='DAM']+1
complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='BMW'] <- complianceTrucks2012$labPosX[complianceTrucks2012$manufacturer=='BMW']+1.5

#####################################################################################################

#####################################################################################################
#EIA Projections
projections <- read.csv('inputs/eia_projection.csv')
eia.2012 <- projections[,1:5]
eia.2013 <- projections[,c(1,6:9)]
eia.2014 <- projections[,c(1,10:13)]
eia.2015 <- projections[,c(1,14:17)]
epa <- projections[,c(1,18:21)]
colnames(eia.2012)=c("Year","FFV","PHEV10","PHEV40","BEV100")
colnames(eia.2013)=c("Year","FFV","PHEV10","PHEV40","BEV100")
colnames(eia.2014)=c("Year","FFV","PHEV10","PHEV40","BEV100")
colnames(eia.2015)=c("Year","FFV","PHEV10","PHEV40","BEV100")
colnames(epa)=c("Year","FFV","PHEV10","PHEV40","BEV100")

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi5.pdf',height=12,width=12)
par(mar=c(6,6,2,2),mfrow=c(2,2))
plot(x=eia.2012$Year,y=eia.2012$PHEV10,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(eia.2012[,2:5]/1000)),main='2012 Projections')
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=3,cex=1.5)
mtext('Forecasted Sales (thousands)',side=2,line=4,cex=1.5)
lines(eia.2012$Year,eia.2012$BEV100/1000,lty=2,lwd=3.5,col='dodgerblue4')
lines(eia.2012$Year,eia.2012$PHEV10/1000,lty=5,lwd=3.5,col="dodgerblue4")
lines(eia.2012$Year,eia.2012$PHEV40/1000,lty=3,lwd=3.5,col="dodgerblue4")
lines(eia.2012$Year,eia.2012$FFV/1000,lty=4,lwd=3.5,col="dodgerblue4")
text(x=c(2021,2016,2015,2017),y=c(15,95,50,615),pos=c(4,4,2,1),labels=c("BEV-100","PHEV-10","PHEV-40","FFV"),cex=1.3)
plot(x=eia.2013$Year,y=eia.2013$PHEV10,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(eia.2012[,2:5]/1000)),main='2013 Projections')
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=3,cex=1.5)
mtext('Forecasted Sales (thousands)',side=2,line=4,cex=1.5)
lines(eia.2013$Year,eia.2013$BEV100/1000,lty=2,lwd=3.5,col='red3')
lines(eia.2013$Year,eia.2013$PHEV10/1000,lty=5,lwd=3.5,col="red3")
lines(eia.2013$Year,eia.2013$PHEV40/1000,lty=3,lwd=3.5,col="red3")
lines(eia.2013$Year,eia.2013$FFV/1000,lty=4,lwd=3.5,col="red3")
text(x=c(2015,2015,2022,2018),y=c(20,95,80,200),pos=c(4,4,4,1),labels=c("BEV-100","PHEV-10","PHEV-40","FFV"),cex=1.3)
plot(x=eia.2014$Year,y=eia.2014$PHEV10,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(eia.2012[,2:5]/1000)),main='2014 Projections')
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=3,cex=1.5)
mtext('Forecasted Sales (thousands)',side=2,line=4,cex=1.5)
lines(eia.2014$Year,eia.2014$BEV100/1000,lty=2,lwd=3.5,col='forestgreen')
lines(eia.2014$Year,eia.2014$PHEV10/1000,lty=5,lwd=3.5,col="forestgreen")
lines(eia.2014$Year,eia.2014$PHEV40/1000,lty=3,lwd=3.5,col="forestgreen")
lines(eia.2014$Year,eia.2014$FFV/1000,lty=4,lwd=3.5,col="forestgreen")
text(x=c(2013,2016,2022,2018),y=c(120,195,80,450),pos=c(4,4,4,1),labels=c("BEV-100","PHEV-10","PHEV-40","FFV"),cex=1.3)
arrows(x0=2015,y0=100,x1=2016.2,y1=13,lwd=.8,len=.1)
arrows(x0=2017,y0=190,x1=2018,y1=30,lwd=.8,len=.1)
plot(x=eia.2015$Year,y=eia.2015$PHEV10,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(eia.2012[,2:5]/1000)),main='2015 Projections')
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=3,cex=1.5)
mtext('Forecasted Sales (thousands)',side=2,line=4,cex=1.5)
lines(eia.2015$Year,eia.2015$BEV100/1000,lty=2,lwd=3.5,col='black')
lines(eia.2015$Year,eia.2015$PHEV10/1000,lty=5,lwd=3.5,col="black")
lines(eia.2015$Year,eia.2015$PHEV40/1000,lty=3,lwd=3.5,col="black")
lines(eia.2015$Year,eia.2015$FFV/1000,lty=4,lwd=3.5,col="black")
arrows(x0=2015,y0=100,x1=2016.2,y1=13,lwd=.8,len=.1)
arrows(x0=2017,y0=190,x1=2018,y1=30,lwd=.8,len=.1)
text(x=c(2013,2016,2022,2018),y=c(120,195,80,400),pos=c(4,4,4,1),labels=c("BEV-100","PHEV-10","PHEV-40","FFV"),cex=1.3)
dev.off()

#####################################################################################################

#####################################################################################################
#####################################################################################################
#Modelling results
#####################################################################################################
#####################################################################################################
#Global inputs
#read inputs
input <- read.csv("inputs/input.csv")
constants <- read.csv('inputs/constants.csv')
new <- read.csv('inputs/newcar_lca.csv')
lca.input <- read.csv('inputs/input3.csv')
index <- read.csv('inputs/index.csv')

#ordering
new <- merge(new,index,by.x="type",by.y="index")
lca.input <- merge(lca.input,index,by.x="type",by.y="index")
new <- new[sort.list(new$id,decreasing=FALSE),]
input <- merge(input,index,by.x="type",by.y="index")

#####################################################################################################

#####################################################################################################

#####################################################################################################


#footprint to emrate req calculator
emRateCalculator <- function(footprintInput,yearInput,carType) {
	if(carType=='car') {
		abcd <- read.csv('inputs/abcd_cars.csv')
		col <- yearInput-2010
		if(footprintInput < 41) {output <- abcd[1,col]}
		else if(footprintInput >= 41 && footprintInput < 56) {output <- abcd[3,col]*footprintInput+abcd[4,col]}
		else if(footprintInput >= 56) {output <- abcd[2,col]}
	}
	else if(carType=='truck') {
		abcd <- read.csv('inputs/abcd_trucks.csv')
		col <- yearInput-2010
		if(footprintInput < 41) {output <- abcd[1,col]}
		else if(footprintInput >= 41 && footprintInput < 61 && yearInput <2017) {output <- abcd[3,col]*footprintInput+abcd[4,col]}
		else if(footprintInput >= 41 && footprintInput < 61 && yearInput >= 2017) {output <- min(abcd[3,col]*footprintInput+abcd[4,col],abcd[7,col]*footprintInput+abcd[8,col])}
		else if(footprintInput >= 66) {output <- abcd[2,col]}
	}
	return(output)
}

kWhPer100MileToEmRate <- function(kWhInput,footprintInput,carType,margEm=.534) {
	output <- numeric(0)
	for(year in 2012:2025) {
		comparableICVEmrate <- emRateCalculator(footprintInput,year,carType)/8887*2471
		hold <- kWhInput*10/.935*margEm-comparableICVEmrate
		output <- c(output,hold)
	}
	return(output)
}

emRatesInc <- read.csv('inputs/em_rate_example.csv')

#this function creates the annual gasoline emissions rate for select vehicles
generateGasEmRate <- function(dataInput) {
	output <- numeric(0)
	for(row in 1:nrow(dataInput)) {
		if(dataInput$type[row]=='BEV') {hold <- rep(0,length(2012:2025))}
		else {hold <- rep(8887/dataInput$twoCycGas[row],length(2012:2025))}
		output <- rbind(output,hold)
	}
	return(output)
}

#this function creates the annual alternative fuel emissions rate for select vehicles
generateAltEmRate <- function(dataInput,margEm=.534) {
	output <- numeric(0)
	for(row in 1:nrow(dataInput)) {
		if(dataInput$type[row]=='BEV'|dataInput$type[row]=='PHEV') {
			hold <- kWhPer100MileToEmRate(dataInput$twoCycAlt[row],dataInput$footprint[row],dataInput$carType[row],margEm)
		}
		else {hold <- rep(8887/dataInput$twoCycAlt[row],length(2012:2025))}
		output <- rbind(output,hold)
	}
	return(output)
}

#this function creates the total annual emissions rate for select vehicles (as would be the actual emissions requirements for EPA)
generateTotEmRate <- function(dataInput) {
	output <- dataInput$proportion*generateAltEmRate(dataInput)+(1-dataInput$proportion)*generateGasEmRate(dataInput)
	return(output)
}

#chevy volt example
#generate data frame of inputs
chevyVoltExample <- data.frame('Year'=2012:2025,'Standards'=standards$passengercars,'VoltNoInc'=generateTotEmRate(emRatesInc)[5,],'VoltInc'=generateGasEmRate(emRatesInc)[5,]*(1-emRatesInc$proportion[5]))
chevyVoltExample$BalancingInc <- chevyVoltExample$Standards*(1+constants$mult.phev)-chevyVoltExample$VoltInc*constants$mult.phev
chevyVoltExample$BalancingNoInc <- chevyVoltExample$Standards*2-chevyVoltExample$VoltNoInc

red.shade = rgb(1,0,0,alpha=0.55,maxColorValue=1)

co2scale <- data.frame('gCO2.per.mile'=seq(0,400,by=100))
co2scale$gal.per100.mi <- co2scale$gCO2.per.mile/8887*100

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figure2.pdf',height=7,width=10)
par(mar=c(6,6,2,6))
plot(x=chevyVoltExample$Year,y=chevyVoltExample$Standards,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',xlim=c(2011.5,2025),ylim=c(0,max(chevyVoltExample[,c(2:6)])))
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
axis(4,at=co2scale$gCO2.per.mile,labels=round(co2scale$gal.per100.mi,2),las=2,tck=.02,cex.axis=1.5)
mtext('Year of Vehicle Purchase',side=1,line=3,cex=1.5)
mtext(expression('Emission Rate (grams CO'[2]~'/mile)'),side=2,line=4,cex=1.5)
mtext('Fuel Consumption (gallons/100 mi)',side=4,las=0,line=4.5,cex=1.5)
grid()
polygon(c(chevyVoltExample$Year,rev(chevyVoltExample$Year)),c(chevyVoltExample$BalancingNoInc,rev(chevyVoltExample$BalancingInc)),col=red.shade,border=NA)
lines(x=chevyVoltExample$Year,y=chevyVoltExample$Standards,col='black',lwd=2)
points(x=chevyVoltExample$Year,y=chevyVoltExample$Standards,col='black',pch=16)
text(x=2012,y=240,pos=4,labels="GHG Emissions Standard",srt=-10,cex=1.2)
lines(x=chevyVoltExample$Year,y=chevyVoltExample$VoltNoInc,col='blue',lwd=2)
points(x=chevyVoltExample$Year,y=chevyVoltExample$VoltNoInc,col='blue',pch=16)
text(x=2012,y=120,pos=4,labels="Volt emissions (true)",srt=2,cex=1.2)
lines(x=chevyVoltExample$Year,y=chevyVoltExample$BalancingNoInc,col='red',lwd=2)
points(x=chevyVoltExample$Year,y=chevyVoltExample$BalancingNoInc,col='red',pch=16)
text(x=2012,y=380,pos=4,labels="Balancing vehicle emissions, no \nAFV incentives",srt=-24,cex=1.2)
lines(x=chevyVoltExample$Year,y=chevyVoltExample$VoltInc,col='blue',lwd=2,lty=2)
points(x=chevyVoltExample$Year,y=chevyVoltExample$VoltInc,col='blue',pch=16)
text(x=2012,y=40,pos=4,labels="Volt emissions modified via weights",cex=1.2)
lines(x=chevyVoltExample$Year,y=chevyVoltExample$BalancingInc,col='red',lwd=2,lty=2)
points(x=chevyVoltExample$Year,y=chevyVoltExample$BalancingInc,col='red',pch=16)
text(x=2018,y=445,pos=4,labels="Balancing vehicle emissions,\nwith AFV incentives",srt=-24,cex=1.2)
text(x=2018,y=350,labels=expression(bar(paste(Delta,italic('r'),sep=''))['B']),font=3,family='serif',cex=1.5)
text(x=2012.2,y=470,pos=2,labels=expression(paste(italic(bar('r'))['B'],'\'')),font=3,family='serif',cex=1.5)
text(x=2012,y=420,pos=2,labels=expression(italic(bar('r'))['B']),font=3,family='serif',cex=1.5)
text(x=2012,y=265,pos=2,labels=expression(italic(bar('s'))),font=3,family='serif',cex=1.5)
text(x=2016,y=125,pos=4,labels=expression(paste(italic('p'['j']),italic('r'['j'])^'A','+(',italic('1-p'['j']),')',italic('r'['j'])^'G',sep='')),font=3,family='serif',cex=1.5,srt=2)
text(x=2018,y=35,pos=4,labels=expression(paste(italic('w'['j']),italic('p'['j']),italic('r'['j'])^'A','+(',italic('1-p'['j']),')',italic('r'['j'])^'G',sep='')),font=3,family='serif',cex=1.5)
dev.off()

#example for other vehicles
generateEmIncrease <- function(inputRow,margEm=.534) {
	techType <- inputRow[2]
	carType <- inputRow[9]
	if(carType=='car') {standardsHold <- standards$passengercars}
	else if(carType=='truck') {standardsHold <- standards$trucks}
	if(techType=='FFV') {
		weightsHold <- constants$weight.ffv
		multHold <- constants$mult.ffv
	}
	else if(techType=='BEV') {
		weightsHold <- constants$weight.ev
		multHold <- constants$mult.ev
	}
	else if(techType=='PHEV') {
		weightsHold <- constants$weight.phev
		multHold <- constants$mult.phev
	}
	proportionHold <- as.numeric(inputRow[8])
	altEmRate <- generateAltEmRate(inputRow,margEm)
	gasEmRate <- generateGasEmRate(inputRow)
	balancingVector <- standardsHold*(multHold-1)+(1-weightsHold*multHold)*rep(proportionHold,length(2012:2025))*altEmRate+(1-multHold)*(rep(1-proportionHold,length(2012:2025))*gasEmRate)
	return(as.vector(balancingVector))
}

generateEmIncreaseDataFrame <- function(inputData,margEm=.534) {
	hold <- data.frame('Year'=c(2012:2025))
	for(row in 1:nrow(inputData)) {
		hold <- cbind(hold,generateEmIncrease(inputData[row,],margEm)*156793/10^6)
	}
	colnames(hold) <- c('Year',inputData$model)
	return(hold)
}


#Emissions rate for several example vehicles
exampleForPlot <- generateEmIncreaseDataFrame(emRatesInc)
rainbowColor <- c('black','blue4','red','forestgreen','darkorchid2','darkorange2','darkgoldenrod2')

tonsScale <- data.frame('tons'=seq(0,60,by=10))
tonsScale$gallons <- tonsScale$tons/8887*10^3

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figure3.pdf',height=7,width=10)
par(mar=c(6,7,2,7))
plot(x=exampleForPlot$Year,y=exampleForPlot[,2],type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(exampleForPlot[,c(2:8)])))
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
axis(4,at=tonsScale$tons,labels=round(tonsScale$gallons,1),las=2,tck=.02,cex.axis=1.5)
mtext('Year of Vehicle Sale',side=1,line=3,cex=1.5)
mtext(bquote(atop('Net Increase in US CO'[2]~' Emissions','Per Vehicle Substituted (tons)')),side=2,line=3,cex=1.5)
mtext('Net increase in US Gasoline Consumption\nPer Vehicle Substituted (thousands of gal)',side=4,line=5,cex=1.5)
for(col in 2:ncol(exampleForPlot)) {
	lines(x=exampleForPlot$Year,y=exampleForPlot[,col],lwd=2,col=rainbowColor[col-1])
	points(x=exampleForPlot$Year,y=exampleForPlot[,col],pch=16,col=rainbowColor[col-1])
}
text(x=2016,y=30,pos=2,labels='Impala (FFV)',cex=1.2)
text(x=2022,y=59,pos=4,labels='Focus (BEV)',cex=1.2)
lines(x=c(2017,2022),y=c(exampleForPlot[6,3],59))
text(x=2022,y=49,pos=4,labels='MiEV (BEV)',cex=1.2)
lines(x=c(2018,2022),y=c(exampleForPlot[7,7],49))
text(x=2022,y=39,pos=4,labels='Leaf (BEV)',cex=1.2)
lines(x=c(2019,2022),y=c(exampleForPlot[8,4],39))
text(x=2016.5,y=45,pos=2,labels='RAV4 (BEV)',cex=1.2)
text(x=2021,y=5,pos=2,labels='Prius (PHEV)',cex=1.2)
text(x=2016.75,y=25,pos=4,labels='Volt (PHEV)',cex=1.2)
dev.off()

#lifetime emissions, total emissions/mi for all cars sold in each year
generateEmIncreaseByTech <- function(margEm=.534) {
	hold <- data.frame('Year'=c(2012:2025),'FFV'=generateEmIncrease(emRatesInc[1,],margEm),'PHEV10'=generateEmIncrease(emRatesInc[4,],margEm),'PHEV40'=generateEmIncrease(emRatesInc[5,],margEm),'BEV100'=generateEmIncrease(emRatesInc[3,],margEm))
	eia2012Out <- hold[,c(2:5)]*eia.2012[,c(2:5)]
	eia2013Out <- hold[,c(2:5)]*eia.2013[,c(2:5)]
	eia2014Out <- hold[,c(2:5)]*eia.2014[,c(2:5)]
	eia2015Out <- hold[,c(2:5)]*eia.2015[,c(2:5)]
	epaOut <- hold[,c(2:5)]*epa[,c(2:5)]
	return(list(eia2012Out,eia2013Out,eia2014Out,eia2015Out,epaOut))
}

generateAnnualEmissions <- function(margEm=.534) {
	milesPerYearByAge <- read.table('inputs/mileageByAge.csv',sep=',',header=TRUE)
	emRates <- generateEmIncreaseByTech(margEm)
	output <- data.frame('Year'=2012:2038,'eia2012FFV'=NA,'eia2012PHEV10'=NA,'eia2012PHEV40'=NA,'eia2012BEV100'=NA,'eia2013FFV'=NA,'eia2013PHEV10'=NA,'eia2013PHEV40'=NA,'eia2013BEV100'=NA,'eia2014FFV'=NA,'eia2014PHEV10'=NA,'eia2014PHEV40'=NA,'eia2014BEV100'=NA,'eia2015FFV'=NA,'eia2015PHEV10'=NA,'eia2015PHEV40'=NA,'eia2015BEV100'=NA,'epaFFV'=NA,'epaPHEV10'=NA,'epaPHEV40'=NA,'epaBEV100'=NA)
	for(lists in 1:length(emRates)) {
		for(col in 1:ncol(emRates[[lists]])) {
			yearMarker <- 0
			outerHold <- rep(0,27)
			for(row in 1:nrow(emRates[[lists]])) {
				hold <- emRates[[lists]][row,col]*milesPerYearByAge$Miles/10^6
				hold <- c(rep(0,yearMarker),hold,rep(0,14-yearMarker))
				outerHold <- outerHold+hold
				yearMarker <- yearMarker+1
			}
			output[,(lists-1)*4+col+1] <- outerHold
		}
	}
	output <- output[-nrow(output),]
	return(output)
}

cumulativeEmissions <- cumsum(generateAnnualEmissions())
cumulativeEmissions$Year <- 2012:2037
carList <- c('FFV','PHEV10','PHEV40','BEV100')

gray.shade = rgb(.8,.8,.8,alpha=0.4,maxColorValue=1)

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi4.pdf')
par(mfrow=c(2,2),mar=c(6,6,2,2))
for(col in 1:4) {
	plot(x=cumulativeEmissions$Year,y=cumulativeEmissions[,1]/10^6,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(cumulativeEmissions)/10^6+10),main=carList[col])
	axis(1,las=0,tck=.02,cex.axis=1.5)
	axis(2,las=2,tck=.02,cex.axis=1.5)
	mtext('Year',side=1,line=2.5,cex=1.2)
	mtext(expression('Cumulative Emissions\n(millions of tons of CO'[2]~')'),side=2,line=3,cex=1.2)
	polygon(c(2012,2012,2025,2025),c(0,200,200,0),col=gray.shade,border=NA)
	abline(v=2025,lty=1)
	grid()
	lines(x=cumulativeEmissions$Year,y=cumulativeEmissions[,col+1]/10^6,col='blue4',lwd=2)
	lines(x=cumulativeEmissions$Year,y=cumulativeEmissions[,col+5]/10^6,col='red',lwd=2)
	lines(x=cumulativeEmissions$Year,y=cumulativeEmissions[,col+9]/10^6,col='forestgreen',lwd=2)
	lines(x=cumulativeEmissions$Year,y=cumulativeEmissions[,col+13]/10^6,col='black',lwd=2)
	if(col==1) {
		text(x=2021.5,y=35,pos=2,labels='AEO 2012')
		text(x=2037,y=15,pos=2,labels='AEO 2013')
		text(x=2037,y=39,pos=2,labels='AEO 2014')
		text(x=2034,y=33,pos=2,labels='AEO 2015')
	}
	text(x=2018,y=55,pos=1,labels='Period of AFV\nIncentives',cex=.8)
	text(x=2031.5,y=55,pos=1,labels='Future emissions\nfrom veh operation',cex=.8)
}
dev.off()

cumulativeEmissions$eia2012Total <- rowSums(cumulativeEmissions[,2:5])
cumulativeEmissions$eia2013Total <- rowSums(cumulativeEmissions[,6:9])
cumulativeEmissions$eia2014Total <- rowSums(cumulativeEmissions[,10:13])
cumulativeEmissions$eia2015Total <- rowSums(cumulativeEmissions[,14:17])
cumulativeEmissions$epa <- rowSums(cumulativeEmissions[,18:21])

totalTonsScale <- data.frame('tons'=seq(0,80,by=20))
totalTonsScale$gallons <- totalTonsScale$tons/8887*10^3

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi3.pdf',width=10,height=7)
par(mar=c(6,6,2,6))
plot(x=cumulativeEmissions$Year,y=cumulativeEmissions[,1]/10^6,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(cumulativeEmissions/10^6)+15))
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
axis(4,at=totalTonsScale$tons,labels=round(totalTonsScale$gallons,1),las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=4,cex=1.5)
mtext(expression('Cumulative Emissions\n(millions of tons of CO'[2]~')'),side=2,line=3,cex=1.5)
mtext('Cumulative Gasoline Consumption\n(billions of gal)',side=4,line=5,cex=1.5)
polygon(c(2012,2012,2025,2025),c(0,250,250,0),col=gray.shade,border=NA)
abline(v=2025,lty=1)
grid()
lines(x=cumulativeEmissions$Year,y=cumulativeEmissions$eia2012Total/10^6,col='blue4',lwd=2)
lines(x=cumulativeEmissions$Year,y=cumulativeEmissions$eia2013Total/10^6,col='red',lwd=2)
lines(x=cumulativeEmissions$Year,y=cumulativeEmissions$eia2014Total/10^6,col='forestgreen',lwd=2)
lines(x=cumulativeEmissions$Year,y=cumulativeEmissions$eia2015Total/10^6,col='black',lwd=2)
text(x=2037,y=70,pos=2,labels='AEO 2012',cex=1.2)
text(x=2037,y=25,pos=2,labels='AEO 2013',cex=1.2)
text(x=2037,y=60,pos=2,labels='AEO 2014',cex=1.2)
text(x=2037,y=45,pos=2,labels='AEO 2015',cex=1.2)
text(x=2018,y=85,pos=1,labels='Period of AFV\nIncentives',cex=1.2)
text(x=2031.5,y=85,pos=1,labels='Future emissions\nfrom vehicle operation',cex=1.2)
dev.off()

generateScenarios <- function() {
	highEm <- .786
	lowEm <- .464
	output <- data.frame('Year'=2012:2037)
	output$Mid <- rowSums(cumsum(generateAnnualEmissions())[,14:17])
	output$Low <- rowSums(cumsum(generateAnnualEmissions(lowEm))[,14:17])
	output$High <- rowSums(cumsum(generateAnnualEmissions(highEm))[,14:17])
	return(output)
}

scenarioExamples <- generateScenarios()

totalTonsScale <- data.frame('tons'=seq(0,80,by=20))
totalTonsScale$gallons <- totalTonsScale$tons/8887*10^3

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figure4.pdf',height=7,width=10)
par(mar=c(6,6,2,6))
plot(x=scenarioExamples$Year,y=scenarioExamples[,2]/10^6,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(scenarioExamples/10^6)+20))
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
axis(4,at=totalTonsScale$tons,labels=round(totalTonsScale$gallons,1),las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=4,cex=1.5)
mtext(expression('Cumulative Emissions\n(millions of tons of CO'[2]~')'),side=2,line=3,cex=1.5)
mtext('Cumulative Gasoline Consumption\n(billions of gal)',side=4,line=5,cex=1.5)
polygon(c(2012,2012,2025,2025),c(0,200,200,0),col=gray.shade,border=NA)
abline(v=2025,lty=1)
lines(x=scenarioExamples$Year,y=scenarioExamples$Mid/10^6,col='black',lwd=2,lty=1)
lines(x=scenarioExamples$Year,y=scenarioExamples$Low/10^6,col='black',lwd=2,lty=2)
lines(x=scenarioExamples$Year,y=scenarioExamples$High/10^6,col='black',lwd=2,lty=3)
text(x=2018,y=80,pos=1,labels='Period of AFV\nIncentives',cex=1.2)
text(x=2031.5,y=80,pos=1,labels='Future emissions\nfrom vehicle operation',cex=1.2)
text(x=2030,y=57,pos=2,labels='High',cex=1.4)
text(x=2035,y=53,pos=2,labels='Base Case',cex=1.4)
text(x=2030,y=45,pos=4,labels='Low',cex=1.4)
dev.off()

totalSales <- read.csv('inputs/aeoTotal.csv')

diffStandardEmissions <- function(percentChange,salesInput) {
	ghgStandards <- standards$passengercars
	ghgStandardsChange <- ghgStandards*percentChange
	output <- sum(salesInput*(ghgStandardsChange-ghgStandards)*156793/10^12)
	return(output)
}

nhtsaEmissionsByCar <- function(input) {
	output <- data.frame('model'=paste(input$model,'\n(',input$type,')',sep=''),'modelAbbr'=c('Impala (FFV)','Focus (BEV)','Leaf (BEV)','Prius (PHEV)','Volt (PHEV)','MiEV (BEV)','RAV4 (BEV)'),'proportion'=input$proportion)
	hold <- numeric()
	for(row in 1:nrow(input)) {
		if(input$type[row]=='FFV') {
			hold <- c(hold,input$twoCycAlt[row])
		}
		else {hold <- c(hold,33705/input$twoCycAlt[row]/10)}
	}
	output$mpg <- hold
	output$gas.consumption <- .85*output$proportion/output$mpg
	output$em.rate <- 8887*output$gas.consumption
	output$total.em <- output$em.rate*156793/10^6
	return(output)
}

cafeConstrainedEmByCar <- nhtsaEmissionsByCar(emRatesInc)


tonsScale2 <- data.frame('tons'=seq(0,30,by=5))
tonsScale2$gallons <- tonsScale2$tons/8887*10^3

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi6.pdf',height=7,width=10)
par(mar=c(6,7,2,7))
plot(x=2012:2025,y=rep(max(cafeConstrainedEmByCar$total.em),length(2012:2025)),type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(cafeConstrainedEmByCar$total.em)+2),xlim=c(2010,2025))
grid()
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
axis(4,at=tonsScale2$tons,labels=round(tonsScale2$gallons,1),las=2,tck=.02,cex.axis=1.5)
mtext('Year of Vehicle Sale',side=1,line=3,cex=1.5)
mtext(bquote(atop('Net Increase in US CO'[2]~' Emissions','Per Vehicle Substituted (tons)')),side=2,line=3,cex=1.5)
mtext('Net increase in US Gasoline Consumption\nPer Vehicle Substituted (thousands of gal)',side=4,line=5,cex=1.5)
for(row in 1:nrow(cafeConstrainedEmByCar)) {
	if(row==2) {
		position <- 3
	}
	else if(row==3) {
		position <- 1
	}
	else {
		position <- 2
	}
	if(row %in% c(1,4,5)) {
		lines(x=2012:2025,y=c(rep(cafeConstrainedEmByCar$total.em[row],length(2012:2019)),rep(0,length(2020:2025))),lwd=2,col=rainbowColor[row])
		points(x=2012:2025,y=c(rep(cafeConstrainedEmByCar$total.em[row],length(2012:2019)),rep(0,length(2020:2025))),pch=16,col=rainbowColor[row])		
	}
	else {
		lines(x=2012:2025,y=c(rep(cafeConstrainedEmByCar$total.em[row],length(2012:2025))),lwd=2,col=rainbowColor[row])
		points(x=2012:2025,y=c(rep(cafeConstrainedEmByCar$total.em[row],length(2012:2025))),pch=16,col=rainbowColor[row])
	}
	text(x=2012,y=cafeConstrainedEmByCar$total.em[row],pos=position,labels=cafeConstrainedEmByCar$modelAbbr[row],cex=.8)
}
dev.off()

generateEmIncreaseByTechNHTSA <- function(input) {
	eia2012Total <- data.frame('FFV'=eia.2012$FFV*input$total.em[1],'PHEV10'=eia.2012$PHEV10*input$total.em[4],'PHEV40'=eia.2012$PHEV40*input$total.em[5],'BEV100'=eia.2012$BEV100*input$total.em[3])
	eia2013Total <- data.frame('FFV'=eia.2013$FFV*input$total.em[1],'PHEV10'=eia.2013$PHEV10*input$total.em[4],'PHEV40'=eia.2013$PHEV40*input$total.em[5],'BEV100'=eia.2013$BEV100*input$total.em[3])
	eia2014Total <- data.frame('FFV'=eia.2014$FFV*input$total.em[1],'PHEV10'=eia.2014$PHEV10*input$total.em[4],'PHEV40'=eia.2014$PHEV40*input$total.em[5],'BEV100'=eia.2014$BEV100*input$total.em[3])
	eia2015Total <- data.frame('FFV'=eia.2015$FFV*input$total.em[1],'PHEV10'=eia.2015$PHEV10*input$total.em[4],'PHEV40'=eia.2015$PHEV40*input$total.em[5],'BEV100'=eia.2015$BEV100*input$total.em[3])
	output <- list(eia2012Total,eia2013Total,eia2014Total,eia2015Total)
	for(i in 1:length(output)){
		output[[i]]$FFV[9:14] <- 0
		output[[i]]$PHEV10[9:14] <- 0
		output[[i]]$PHEV40[9:14] <- 0
	}
	return(output)
}

generateAnnualEmissionsNHTSA <- function(input) {
	milesPerYearByAge <- read.table('inputs/mileageByAge.csv',sep=',',header=TRUE)
	emRates <- generateEmIncreaseByTechNHTSA(input)
	output <- data.frame('Year'=2012:2038,'eia2012FFV'=NA,'eia2012PHEV10'=NA,'eia2012PHEV40'=NA,'eia2012BEV100'=NA,'eia2013FFV'=NA,'eia2013PHEV10'=NA,'eia2013PHEV40'=NA,'eia2013BEV100'=NA,'eia2014FFV'=NA,'eia2014PHEV10'=NA,'eia2014PHEV40'=NA,'eia2014BEV100'=NA,'eia2015FFV'=NA,'eia2015PHEV10'=NA,'eia2015PHEV40'=NA,'eia2015BEV100'=NA)
	for(lists in 1:length(emRates)) {
		for(col in 1:ncol(emRates[[lists]])) {
			yearMarker <- 0
			outerHold <- rep(0,27)
			for(row in 1:nrow(emRates[[lists]])) {
				hold <- emRates[[lists]][row,col]*milesPerYearByAge$Miles/156793
				hold <- c(rep(0,yearMarker),hold,rep(0,14-yearMarker))
				outerHold <- outerHold+hold
				yearMarker <- yearMarker+1
			}
			output[,(lists-1)*4+col+1] <- outerHold
		}
	}
	output <- output[-nrow(output),]
	return(output)
}

cumulativeEmissionsNHTSA <- cumsum(generateAnnualEmissionsNHTSA(cafeConstrainedEmByCar))
cumulativeEmissionsNHTSA$Year <- 2012:2037
carList <- c('FFV','PHEV10','PHEV40','BEV100')

gray.shade = rgb(.8,.8,.8,alpha=0.4,maxColorValue=1)

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi8.pdf')
par(mfrow=c(2,2),mar=c(6,6,2,2))
for(col in 1:4) {
	plot(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA[,1]/10^6,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(cumulativeEmissionsNHTSA)/10^6+30),main=carList[col])
	axis(1,las=0,tck=.02,cex.axis=1.5)
	axis(2,las=2,tck=.02,cex.axis=1.5)
	mtext('Year',side=1,line=2.5,cex=1.2)
	mtext(expression('Cumulative Emissions\n(millions of tons of CO'[2]~')'),side=2,line=3,cex=1.2)
	polygon(c(2012,2012,2025,2025),c(0,300,300,0),col=gray.shade,border=NA)
	abline(v=2025,lty=1)
	grid()
	lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA[,col+1]/10^6,col='blue4',lwd=2)
	lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA[,col+5]/10^6,col='red',lwd=2)
	lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA[,col+9]/10^6,col='forestgreen',lwd=2)
	lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA[,col+13]/10^6,col='black',lwd=2)
	if(col==1) {
		text(x=2023,y=80,pos=2,labels='AEO 2012')
		text(x=2037,y=20,pos=2,labels='AEO 2013')
		text(x=2037,y=65,pos=2,labels='AEO 2014')
		text(x=2037,y=95,pos=2,labels='AEO 2015')
	}
	text(x=2018,y=140,pos=1,labels='Period of AFV\nIncentives',cex=.8)
	text(x=2031.5,y=140,pos=1,labels='Future emissions\nfrom veh operation',cex=.8)
}
dev.off()

cumulativeEmissionsNHTSA$eia2012Total <- rowSums(cumulativeEmissionsNHTSA[,2:5])
cumulativeEmissionsNHTSA$eia2013Total <- rowSums(cumulativeEmissionsNHTSA[,6:9])
cumulativeEmissionsNHTSA$eia2014Total <- rowSums(cumulativeEmissionsNHTSA[,10:13])
cumulativeEmissionsNHTSA$eia2015Total <- rowSums(cumulativeEmissionsNHTSA[,14:17])

totalTonsScale <- data.frame('tons'=seq(0,250,by=50))
totalTonsScale$gallons <- totalTonsScale$tons/8887*10^3

pdf('~/Dropbox/CarnegieMellon/Research/Project4_CAFEavis/JournalSubmissions/es&t/figures/figuresi7.pdf',width=10,height=7)
par(mar=c(6,6,2,6))
plot(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA[,1]/10^6,type='n',xlab=NA,ylab=NA,xaxt='n',yaxt='n',ylim=c(0,max(cumulativeEmissionsNHTSA/10^6)+15))
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
axis(4,at=totalTonsScale$tons,labels=round(totalTonsScale$gallons,1),las=2,tck=.02,cex.axis=1.5)
mtext('Year',side=1,line=4,cex=1.5)
mtext(expression('Cumulative Emissions\n(millions of tons of CO'[2]~')'),side=2,line=3,cex=1.5)
mtext('Cumulative Gasoline Consumption\n(billions of gal)',side=4,line=5,cex=1.5)
polygon(c(2012,2012,2025,2025),c(0,300,300,0),col=gray.shade,border=NA)
abline(v=2025,lty=1)
grid()
lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA$eia2012Total/10^6,col='blue4',lwd=2)
lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA$eia2013Total/10^6,col='red',lwd=2)
lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA$eia2014Total/10^6,col='forestgreen',lwd=2)
lines(x=cumulativeEmissionsNHTSA$Year,y=cumulativeEmissionsNHTSA$eia2015Total/10^6,col='black',lwd=2)
text(x=2037,y=108,pos=2,labels='AEO 2012',cex=1.2)
text(x=2037,y=25,pos=2,labels='AEO 2013',cex=1.2)
text(x=2037,y=95,pos=2,labels='AEO 2014',cex=1.2)
text(x=2037,y=75,pos=2,labels='AEO 2015',cex=1.2)
text(x=2018,y=130,pos=1,labels='Period of AFV\nIncentives',cex=1.2)
text(x=2031.5,y=130,pos=1,labels='Future emissions\nfrom vehicle operation',cex=1.2)
dev.off()