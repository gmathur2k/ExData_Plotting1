## Load libraries needed
library(chron)

## Set working directory:
setwd("C:/Users/gagan.mathur/OneDrive/Career/Data Science Specialization/Course 4 Exploratory Data Analysis/Course Project 1")
getwd()

## Take user input for fiename, start/end date range first: 
## My inputs: data_household_power_consumption.txt, plot4.png, 01/02/2007, 02/02/2007
datfilename=readline(prompt="Enter data file name: ")
plot4filename=readline(prompt="Enter Plot 4 PNG file name: ")
startdate=as.Date(readline(prompt="Enter the start date for date to be processed [dd/mm/yyyy]: "), format="%d/%m/%Y")
enddate=as.Date(readline(prompt="Enter the end date for date to be processed [dd/mm/yyyy]: "), format="%d/%m/%Y")

## Get the first date and time present in the full data file:
hpc_row1 <- read.table(datfilename, nrows = 1, header =TRUE, sep =';')
firstdate <- as.Date(hpc_row1$Date[1], format="%d/%m/%Y")

## Calculate start and end line # for the data to be processed/read:

## Start Line: 1 header row+all data for first date (could be partial day) + data for all days upto (not including) startdate + first row of startdate
startline <- 1+(1-as.numeric(times(hpc_row1$Time)))*24*60+as.integer(startdate-firstdate-1)*24*60+1

## End Line: 1 header row+all data for first date (could be partial day) + data for all days upto (including) enddate
endline <- 1+(1-as.numeric(times(hpc_row1$Time)))*24*60+as.integer(enddate-firstdate)*24*60

skiplines <- startline-2; readlines <- endline-startline+1

## Check all values are correct:
firstdate; startdate; enddate; startline; endline; skiplines; readlines

## Read only the data subset that we wish to process: 01/02/2007 to 02/02/2007 - 2880 rows
hh_power_cons <- read.table(datfilename, skip=skiplines, nrows=readlines, header =TRUE, sep =';')
names(hh_power_cons)=c("Date","Time","Global_active_power","Global_reactive_power",
    "Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
class(hh_power_cons); summary(hh_power_cons)

## Make proper data types:
hh_power_cons$newDate <- as.Date(hh_power_cons$Date, format="%d/%m/%Y")
hh_power_cons$DateTime <- strptime(paste(hh_power_cons$Date,hh_power_cons$Time,sep=" "), format="%d/%m/%Y %H:%M:%S")
## class(hh_power_cons$Date); summary(hh_power_cons$Date)
## class(hh_power_cons$Time); summary(hh_power_cons$Time)
class(hh_power_cons$newDate); summary(hh_power_cons$newDate)
class(hh_power_cons$DateTime); summary(hh_power_cons$DateTime)

## Plot 4: Grid of 4 graphs
## pngfilename
png(filename=plot4filename, width=480, height=480, units="px")

par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(1,1,1,0))

## Top-Left Plot:
plot(hh_power_cons$DateTime, hh_power_cons$Global_active_power, type="n", 
     ## main="Global Active Power", 
     xlab="", ylab="Global Active Power"
)
lines(hh_power_cons$DateTime, hh_power_cons$Global_active_power)

## Top-Right Plot:
plot(hh_power_cons$DateTime, hh_power_cons$Voltage, type="n", 
     ## main="Voltage", 
     xlab="datetime", ylab="Voltage"
)
lines(hh_power_cons$DateTime, hh_power_cons$Voltage)

## Bottom-Left Plot:
plot(hh_power_cons$DateTime, hh_power_cons$Sub_metering_1, type="n", 
     ## main="Energy sub metering", 
     ylab="Energy sub metering", xlab="")
lines(hh_power_cons$DateTime, hh_power_cons$Sub_metering_1, col="brown")
lines(hh_power_cons$DateTime, hh_power_cons$Sub_metering_2, col="green")
lines(hh_power_cons$DateTime, hh_power_cons$Sub_metering_3, col="blue")
legend("topright", col=c("brown", "green", "blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=c(1,1), lwd=c(2.5,2.5), cex=0.5)

## Bottom-Right Plot:
plot(hh_power_cons$DateTime, hh_power_cons$Global_reactive_power, type="n", 
     ## main="Global Reactive Power", 
     xlab="datetime", ylab="Global Reactive Power"
)
lines(hh_power_cons$DateTime, hh_power_cons$Global_reactive_power)

dev.off()
