#To know which directory you are currently in
getwd()

#TO change the directory
setwd()
#To change the directory path one must have the new path handy
#Make sure to change all the backward slashes '\' to forward slashes '/' 
setwd('C:/Users/koolb/Desktop/R for PhIr2021')

# Create a new project with this directory and R will start reading all the files in that folder

# To read a .dat or .csv file in that folder use the 'import dataset' function under file
# Below is an example to open the CR300 csv file in that directory. 
DL4_WestDrysite_15.Min <- read.csv("C:/Users/savas/OneDrive - University of Tennessee/Desktop/R for PhIr2021/DL4_WestDrysite_15 Min.csv")

CR300_15min_WestDry_02112022 <- DL4_WestDrysite_15.Min # Creating a duplicate of an existing raw data file
# Instead of doing the above step just create a copy of the file in the folder itself and edit the file to ensure that the first line is the header.
# Then read that file in R including a command that the first line is the header.

#check the classes of the data
sapply(CR300_15min_WestDry_02112022, class)

#Since all of our data is in one file nothing needs to be changed or organized in the data before moving forward to the next step
#Most of the columns except the date columns and few have been recognized as numbers. So that needs to be changed. Below is a command for reading dates
CR300_15min_WestDry_02112022$TIMESTAMP <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilmoisture5cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilmoisture5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilmoisture15cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilmoisture15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilmoisture25cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilmoisture25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilmoisture5cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilmoisture5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilmoisture15cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilmoisture15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilmoisture25cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilmoisture25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilsalinity5cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilsalinity5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilsalinity15cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilsalinity15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilsalinity25cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilsalinity25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilsalinity5cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilsalinity5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilsalinity15cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilsalinity15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soilsalinity25cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soilsalinity25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature5cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature15cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature25cm_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature5cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature15cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature25cm_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

#Similarly for redox sensors and the soil oxygen sensors
CR300_15min_WestDry_02112022$redox_4_1.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_1.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_1.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_1.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_1.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_1.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_1.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_1.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_1.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_WestDry_02112022$redox_4_2.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_2.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_2.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_2.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_2.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_2.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_2.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_2.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_2.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_WestDry_02112022$redox_4_3.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_3.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_3.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_3.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_3.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_3.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_3.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$redox_4_3.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$redox_4_3.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))


CR300_15min_WestDry_02112022$soiloxygen_mg_l_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiloxygen_mg_l_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiloxygen_mg_l_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiloxygen_mg_l_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiloxygen_mA_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiloxygen_mA_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiloxygen_mA_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiloxygen_mA_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature_TMx <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestDry_02112022$soiltemperature_TMn <- as.POSIXct(strptime(CR300_15min_WestDry_02112022$soiltemperature_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))


# I have converted all colummns to numerical and date formats. Now I need to be able to plot the data

# Plot 1: Timestamp vs battery voltage, changing axes labels along with it
plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$batt_volt_Min, xlab = "Timestamp", ylab = "Battery Voltage (V)", main = "West Dry site", ylim = c(5, 15))

#plot 2
#This is one way to plot the different series onto a single plot. Type 'P' here refers to points. Type 'l' refers to line plot.
plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soilmoisture5cm_Avg, type = 'p', main = "West Dry site", xlab='Time', ylab='Soil moisture(%)', col='blue', ylim = c(10, 50))
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soilmoisture15cm_Avg, type='p', col='green')
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soilmoisture25cm_Avg, type='p', col='red')
legend('topleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.82)

plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soiltemperature5cm_Avg, type = 'p', main = "West Dry site", xlab='Time', ylab='Soil temperature(oC)', col='blue')
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soiltemperature15cm_Avg, type='p', col='green')
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soiltemperature25cm_Avg, type='p', col='red')
legend('topright', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soilsalinity5cm_Avg, type = 'p', main = "West Dry site", xlab='Time', ylab='Soil salinity(VIC)', col='blue', ylim = c(900, 1500))
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soilsalinity15cm_Avg, type='p', col='green')
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soilsalinity25cm_Avg, type='p', col='red')
legend('topleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.9)

plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$soiloxygen_mg_l_Avg, type = 'p', main = "West Dry site", xlab='Time', ylab='Soil oxygen (mg/L)', col='blue', ylim = c(0, 12))
legend('topright', legend = c("10cm"), col = c("Blue"), pch=1, cex=1)


# For the redox data. We will need to correct them for standard hydrogen electrode. This is done by adding new columns that calculate the corrected redox data. 
# Fist use names to identify the column numbers 
names(CR300_15min_WestDry_02112022)

#Use the accurate column numbers to calculate this new column.
CR300_15min_WestDry_02112022$Redox_4_1.1_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,60]+273
CR300_15min_WestDry_02112022$Redox_4_1.2_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,61]+273
CR300_15min_WestDry_02112022$Redox_4_1.3_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,62]+273
CR300_15min_WestDry_02112022$Redox_4_1.4_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,63]+273

CR300_15min_WestDry_02112022$Redox_4_2.1_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,84]+273
CR300_15min_WestDry_02112022$Redox_4_2.2_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,85]+273
CR300_15min_WestDry_02112022$Redox_4_2.3_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,86]+273
CR300_15min_WestDry_02112022$Redox_4_2.4_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,87]+273

CR300_15min_WestDry_02112022$Redox_4_3.1_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,108]+273
CR300_15min_WestDry_02112022$Redox_4_3.2_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,109]+273
CR300_15min_WestDry_02112022$Redox_4_3.3_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,110]+273
CR300_15min_WestDry_02112022$Redox_4_3.4_fromtip_Avg_corrected <- CR300_15min_WestDry_02112022[,111]+273


# To check if the values were calculated correctly using the correct column values
CR300_15min_WestDry_02112022[, 156]
CR300_15min_WestDry_02112022[, 157]
CR300_15min_WestDry_02112022[, 158]
CR300_15min_WestDry_02112022[, 159]

CR300_15min_WestDry_02112022[, 160]
CR300_15min_WestDry_02112022[, 161]
CR300_15min_WestDry_02112022[, 162]
CR300_15min_WestDry_02112022[, 163]

CR300_15min_WestDry_02112022[, 164]
CR300_15min_WestDry_02112022[, 165]
CR300_15min_WestDry_02112022[, 166]
CR300_15min_WestDry_02112022[, 167]


# Now that I have all the redox data corrected, I need to plot to see how the data looks. 
par(mar = c(5, 4, 4, 8),
    xpd = TRUE)           # Drawing margin outside the graph area for the legend

plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_1.1_fromtip_Avg_corrected, type = 'p', main = "West Dry site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-500, 1100))
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_1.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_1.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_1.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.15, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)

plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_2.1_fromtip_Avg_corrected, type = 'p', main = "West Dry site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-500, 1100))
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_2.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_2.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_2.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.15, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)

plot(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_3.1_fromtip_Avg_corrected, type = 'p', main = "West Dry site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-500, 1100))
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_3.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_3.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_WestDry_02112022$TIMESTAMP, CR300_15min_WestDry_02112022$Redox_4_3.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.15, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)



# Removing the ref columns as those data sets mean nothing.I saved this as another version. CR300_15min_EastDry_02012022_V1
CR300_15min_WestDry_02112022_V1 <- CR300_15min_WestDry_02112022
CR300_15min_WestDry_02112022_V1[132:137]<-NULL
names(CR300_15min_WestDry_02112022_V1)


#since we plan to combine the data from all 6 data loggers into one data frame, we should make sure all columns have unique names with the datalogger number
#Since the soil moisture, soil salinity, soil temperature, and the soil oxygen data are not labelled for the specific datalogger we should change that.
#This is big change so we should save the data frame in a different name
CR300_15min_WestDry_02112022_V2 <- CR300_15min_WestDry_02112022_V1
colnames(CR300_15min_WestDry_02112022_V2)
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture5cm_Avg"] <- "soilmoisture_4_5cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture15cm_Avg"] <- "soilmoisture_4_15cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture25cm_Avg"] <- "soilmoisture_4_25cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture5cm_Std"] <- "soilmoisture_4_5cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture15cm_Std"] <- "soilmoisture_4_15cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture25cm_Std"] <- "soilmoisture_4_25cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture5cm_Max"] <- "soilmoisture_4_5cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture15cm_Max"] <- "soilmoisture_4_15cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture25cm_Max"] <- "soilmoisture_4_25cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture5cm_TMx"] <- "soilmoisture_4_5cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture15cm_TMx"] <- "soilmoisture_4_15cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture25cm_TMx"] <- "soilmoisture_4_25cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture5cm_Min"] <- "soilmoisture_4_5cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture15cm_Min"] <- "soilmoisture_4_15cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture25cm_Min"] <- "soilmoisture_4_25cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture5cm_TMn"] <- "soilmoisture_4_5cm_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture15cm_TMn"] <- "soilmoisture_4_15cm_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilmoisture25cm_TMn"] <- "soilmoisture_4_25cm_TMn"

names(CR300_15min_WestDry_02112022_V2)

colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity5cm_Avg"] <- "soilsalinity_4_5cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity15cm_Avg"] <- "soilsalinity_4_15cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity25cm_Avg"] <- "soilsalinity_4_25cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity5cm_Std"] <- "soilsalinity_4_5cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity15cm_Std"] <- "soilsalinity_4_15cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity25cm_Std"] <- "soilsalinity_4_25cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity5cm_Max"] <- "soilsalinity_4_5cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity15cm_Max"] <- "soilsalinity_4_15cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity25cm_Max"] <- "soilsalinity_4_25cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity5cm_TMx"] <- "soilsalinity_4_5cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity15cm_TMx"] <- "soilsalinity_4_15cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity25cm_TMx"] <- "soilsalinity_4_25cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity5cm_Min"] <- "soilsalinity_4_5cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity15cm_Min"] <- "soilsalinity_4_15cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity25cm_Min"] <- "soilsalinity_4_25cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity5cm_TMn"] <- "soilsalinity_4_5cm_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity15cm_TMn"] <- "soilsalinity_4_15cm_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soilsalinity25cm_TMn"] <- "soilsalinity_4_25cm_TMn"

colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature5cm_Avg"] <- "soiltemperature_4_5cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature15cm_Avg"] <- "soiltemperature_4_15cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature25cm_Avg"] <- "soiltemperature_4_25cm_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature5cm_Std"] <- "soiltemperature_4_5cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature15cm_Std"] <- "soiltemperature_4_15cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature25cm_Std"] <- "soiltemperature_4_25cm_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature5cm_Max"] <- "soiltemperature_4_5cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature15cm_Max"] <- "soiltemperature_4_15cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature25cm_Max"] <- "soiltemperature_4_25cm_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature5cm_TMx"] <- "soiltemperature_4_5cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature15cm_TMx"] <- "soiltemperature_4_15cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature25cm_TMx"] <- "soiltemperature_4_25cm_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature5cm_Min"] <- "soiltemperature_4_5cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature15cm_Min"] <- "soiltemperature_4_15cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature25cm_Min"] <- "soiltemperature_4_25cm_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature5cm_TMn"] <- "soiltemperature_4_5cm_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature15cm_TMn"] <- "soiltemperature_4_15cm_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature25cm_TMn"] <- "soiltemperature_4_25cm_TMn"

colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mA_Avg"] <- "soiloxygen_4_mA_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mA_Std"] <- "soiloxygen_4_mA_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mA_Max"] <- "soiloxygen_4_mA_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mA_TMx"] <- "soiloxygen_4_mA_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mA_Min"] <- "soiloxygen_4_mA_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mA_TMn"] <- "soiloxygen_4_mA_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mg_l_Std"] <- "soiloxygen_4_mg_l_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mg_l_Max"] <- "soiloxygen_4_mg_l_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mg_l_TMx"] <- "soiloxygen_4_mg_l_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mg_l_Min"] <- "soiloxygen_4_mg_l_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiloxygen_mg_l_TMn"] <- "soiloxygen_4_mg_l_TMn"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature_Avg"] <- "soiltemperature_4_Avg"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature_Std"] <- "soiltemperature_4_Std"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature_Max"] <- "soiltemperature_4_Max"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature_TMx"] <- "soiltemperature_4_TMx"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature_Min"] <- "soiltemperature_4_Min"
colnames(CR300_15min_WestDry_02112022_V2)[colnames(CR300_15min_WestDry_02112022_V2)== "soiltemperature_TMn"] <- "soiltemperature_4_TMn"



