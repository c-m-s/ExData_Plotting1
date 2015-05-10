########################################################################################
##
## Date 2015-05-10
## Written by Carl Smith for Exploratory Data Analysis class
## 
## Software used to test and run script:
## MacOS X 10.10.3
## R version 3.2.0 (2015-04-16) -- "Full of Ingredients"
## RStudio Version 0.98.1103
##
########################################################################################

plot1 <- function() {
        
        ## Your code file should include code for reading the data so that the plot can be
        ## fully reproduced. 
        
        # Check to see if the household_power_consumption.txt file is in the working directory.
        if(!file.exists("household_power_consumption.txt")) {
                
                ## Check if the "exdata data household_power_consumption.zip" file is around
                ## and if not, then download it.
                
                if(!file.exists("household_power_consumption.zip")) {
                        fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                        download.file(fileURL, destfile="household_power_consumption.zip",
                                      method="curl",mode="wb")
                        dataDownloaded <- date()
                        print(paste("Downloaded electric power consumption zip file on", dataDownloaded))
                }
                
                # Unzip the household_power_consumption.txt so it can be read and used to reproduce the plot.
                unzip("household_power_consumption.zip")        
        }
        
        
        
        
        # Make a date class to use to format the date string when read into read.table 
        setClass('myDate')
        setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y"))
        
        
        powerdata <- read.table("household_power_consumption.txt", 
                                header=TRUE, 
                                stringsAsFactors=FALSE, 
                                sep=";",
                                colClasses = c("myDate", "character", "numeric", "numeric", "numeric",
                                               "numeric","numeric", "numeric", "numeric"),
                                na.strings = "?")
        
        ## We will only be using data from the dates 2007-02-01 and 2007-02-02.
        
        # Pull out all of the rows of data from 2007-02-01 and 2007-02-02.
        powerdata2days <- powerdata[powerdata$Date %in% c(as.Date("2007-02-01"), as.Date("2007-02-02")),]
        
        # Add a new column that combines the date field and the time field to use later in plots.
        powerdata2days$datetime <- strptime(paste(powerdata2days$Date, powerdata2days$Time), "%Y-%m-%d %H:%M:%S")
        
        
        
        
        
        ## For each plot you should
        ##   - Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
        ##   - Name each of the plot files as plot1.png, plot2.png, etc.
        
        # general plot setup
        png(file="plot1.png", width = 480, height = 480, units = "px", bg = "transparent")
        
        # generate the plot into the open png graphics device
        hist(powerdata2days$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
        
        # close the png graphics device
        dev.off()


}