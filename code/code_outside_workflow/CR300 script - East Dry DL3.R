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
CR300_FifteenMinData_EastDry <- read.csv("C:/Users/koolb/Desktop/R for PhIr2021/CR300_FifteenMinData_EastDry.dat", header=FALSE)

CR300_15min_EastDry_02012022 <- CR300_FifteenMinData_EastDry # Creating a duplicate of an existing raw data file
# Instead of doing the above step just create a copy of the file in the folder itself and edit the file to ensure that the first line is the header.
# Then read that file in R including a command that the first line is the header.

#check the classes of the data
sapply(CR300_15min_EastDry_02012022, class)

#Since all of our data is in one file nothing needs to be changed or organized in the data before moving forward to the next step
#Most of the columns except the date columns and few have been recognized as numbers. So that needs to be changed. Below is a command for reading dates
CR300_15min_EastDry_02012022$TIMESTAMP <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilmoisture5cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilmoisture5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilmoisture15cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilmoisture15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilmoisture25cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilmoisture25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilmoisture5cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilmoisture5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilmoisture15cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilmoisture15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilmoisture25cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilmoisture25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilsalinity5cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilsalinity5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilsalinity15cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilsalinity15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilsalinity25cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilsalinity25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilsalinity5cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilsalinity5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilsalinity15cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilsalinity15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soilsalinity25cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soilsalinity25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature5cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature15cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature25cm_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature5cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature15cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature25cm_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

#Similarly for redox sensors and the soil oxygen sensors
CR300_15min_EastDry_02012022$redox_3_1.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_1.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_1.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_1.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_1.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_1.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_1.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_1.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_1.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_EastDry_02012022$redox_3_2.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_2.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_2.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_2.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_2.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_2.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_2.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_2.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_2.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_EastDry_02012022$redox_3_3.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_3.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_3.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_3.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_3.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_3.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_3.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$redox_3_3.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$redox_3_3.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))


CR300_15min_EastDry_02012022$soiloxygen_mg_l_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiloxygen_mg_l_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiloxygen_mg_l_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiloxygen_mg_l_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiloxygen_mA_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiloxygen_mA_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiloxygen_mA_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiloxygen_mA_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature_TMx <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastDry_02012022$soiltemperature_TMn <- as.POSIXct(strptime(CR300_15min_EastDry_02012022$soiltemperature_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))


# I have converted all colummns to numerical and date formats. Now I need to be able to plot the data

# Plot 1: Timestamp vs battery voltage, changing axes labels along with it
plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$batt_volt_Min, xlab = "Timestamp", ylab = "Battery Voltage (V)", main = "East Dry site")

#plot 2
#This is one way to plot the different series onto a single plot. Type 'P' here refers to points. Type 'l' refers to line plot.
plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soilmoisture5cm_Avg, type = 'p', main = "East Dry site", xlab='Time', ylab='Soil moisture(%)', col='blue')
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soilmoisture15cm_Avg, type='p', col='green')
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soilmoisture25cm_Avg, type='p', col='red')
legend('bottomleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.82)

plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soiltemperature5cm_Avg, type = 'p', main = "East Dry site", xlab='Time', ylab='Soil temperature(oC)', col='blue')
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soiltemperature15cm_Avg, type='p', col='green')
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soiltemperature25cm_Avg, type='p', col='red')
legend('topright', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.68)

plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soilsalinity5cm_Avg, type = 'p', main = "East Dry site", xlab='Time', ylab='Soil salinity(VIC)', col='blue')
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soilsalinity15cm_Avg, type='p', col='green')
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soilsalinity25cm_Avg, type='p', col='red')
legend('bottomleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$soiloxygen_mg_l_Avg, type = 'p', main = "East Dry site", xlab='Time', ylab='Soil oxygen (mg/L)', col='blue')
legend('topright', legend = c("10cm"), col = c("Blue"), pch=1, cex=1)


# For the redox data. We will need to correct them for standard hydrogen electrode. This is done by adding new columns that calculate the corrected redox data. 
# Fist use names to identify the column numbers 
names(CR300_15min_EastDry_02012022)

#Use the accurate column numbers to calculate this new column.
CR300_15min_EastDry_02012022$Redox_3_1.1_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,60]+273
CR300_15min_EastDry_02012022$Redox_3_1.2_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,61]+273
CR300_15min_EastDry_02012022$Redox_3_1.3_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,62]+273
CR300_15min_EastDry_02012022$Redox_3_1.4_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,63]+273

CR300_15min_EastDry_02012022$Redox_3_2.1_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,84]+273
CR300_15min_EastDry_02012022$Redox_3_2.2_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,85]+273
CR300_15min_EastDry_02012022$Redox_3_2.3_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,86]+273
CR300_15min_EastDry_02012022$Redox_3_2.4_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,87]+273

CR300_15min_EastDry_02012022$Redox_3_3.1_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,108]+273
CR300_15min_EastDry_02012022$Redox_3_3.2_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,109]+273
CR300_15min_EastDry_02012022$Redox_3_3.3_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,110]+273
CR300_15min_EastDry_02012022$Redox_3_3.4_fromtip_Avg_corrected <- CR300_15min_EastDry_02012022[,111]+273

# To check if the values were calculated correctly using the correct column values
CR300_15min_EastDry_02012022[, 156]
CR300_15min_EastDry_02012022[, 157]
CR300_15min_EastDry_02012022[, 158]
CR300_15min_EastDry_02012022[, 159]

CR300_15min_EastDry_02012022[, 160]
CR300_15min_EastDry_02012022[, 161]
CR300_15min_EastDry_02012022[, 162]
CR300_15min_EastDry_02012022[, 163]

CR300_15min_EastDry_02012022[, 164]
CR300_15min_EastDry_02012022[, 165]
CR300_15min_EastDry_02012022[, 166]
CR300_15min_EastDry_02012022[, 167]


# Now that I have all the redox data corrected, I need to plot to see how the data looks. 
par(mar = c(5, 4, 4, 6),
    xpd = TRUE)           # Drawing margin outside the graph area for the legend

plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_1.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-1500, 1100))
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_1.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_1.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_1.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.35, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)

plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_2.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-1500, 1100))
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_2.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_2.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_2.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.37, 0), 
       legend = c("above surface","5cm","15cm","25cm"), 
       col = 4:1, 
       pch=1, cex=0.8)

plot(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_3.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-1500, 1100))
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_3.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_3.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02012022$TIMESTAMP, CR300_15min_EastDry_02012022$Redox_3_3.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.35, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)



# Removing the ref columns as those data sets mean nothing.I saved this as another version. CR300_15min_EastDry_02012022_V1
CR300_15min_EastDry_02012022_V1 <- CR300_15min_EastDry_02012022
CR300_15min_EastDry_02012022_V1[132:137]<-NULL
names(CR300_15min_EastDry_02012022_V1)

#Data cleaning for redox data. Removing the outliers. I created another copy of the file here. CR300_15min_EastDry_02022022_V2
#Step 1 create a new version of the data frame
CR300_15min_EastDry_02022022_V2 <- CR300_15min_EastDry_02012022_V1

#Step 2 is to identify those weird values
CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected
CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected <= 400 # This is the command that identifies the outliers
CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected <= 400] # This gives me all the outliers in this command
CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected <= 400] = NA  # This is where you nullify those data points to NA

#Check if that worked
plot(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-1500, 1100))
points(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.35, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)


CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected
CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected <= -600 # This is the command that identifies the outliers
CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected <= -600] # This gives me all the outliers in this command
CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected <= -600] = NA  # This is where you nullify those data points to NA

CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected
CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected <= -400 # This is the command that identifies the outliers
CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected <= -400] # This gives me all the outliers in this command
CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected <= -400] = NA  # This is where you nullify those data points to NA

CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected
CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected <= -250 # This is the command that identifies the outliers
CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected <= -250] # This gives me all the outliers in this command
CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected[CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected <= -250] = NA  # This is where you nullify those data points to NA

#Replot the Corrected data.
plot(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-500, 900))
points(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02022022_V2$TIMESTAMP, CR300_15min_EastDry_02022022_V2$Redox_3_1.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.15, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)

# Cleaning the soil moisture/temp/salinity data
CR300_15min_EastDry_02022022_V2$soilmoisture5cm_Avg[6900:7014]
CR300_15min_EastDry_02022022_V2$soilmoisture15cm_Avg[6900:7014]
CR300_15min_EastDry_02022022_V2$soilmoisture25cm_Avg[6900:7014]
# The last two points are bad points, so this is an easy fix. 
#Since it is only the last two data points we can delete the entire row for all which will make coding much easier. 
#Since this will be a big change lets create a new version of the data frame.
CR300_15min_EastDry_02022022_V3 <- head(CR300_15min_EastDry_02022022_V2, - 2)  #last two rows deleted 

#Now we need to replot the data
plot(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soilmoisture5cm_Avg, type = 'p', main = "East Dry site", xlab='Time', ylab='Soil moisture(%)', col='blue')
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soilmoisture15cm_Avg, type='p', col='green')
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soilmoisture25cm_Avg, type='p', col='red')
legend('bottomleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.55)

plot(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soiltemperature5cm_Avg, type = 'p', main = "East Dry site", xlab='Time', ylab='Soil temperature(oC)', col='blue')
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soiltemperature15cm_Avg, type='p', col='green')
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soiltemperature25cm_Avg, type='p', col='red')
legend('topright', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.68)

plot(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soilsalinity5cm_Avg, type = 'p', main = "East Dry site", xlab='Time', ylab='Soil salinity(VIC)', col='blue', ylim = c(1150, 1450))
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soilsalinity15cm_Avg, type='p', col='green')
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$soilsalinity25cm_Avg, type='p', col='red')
legend('bottomleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$Redox_3_1.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-500, 900))
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$Redox_3_1.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$Redox_3_1.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02022022_V3$TIMESTAMP, CR300_15min_EastDry_02022022_V3$Redox_3_1.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.35, 0), 
       legend = c("5cm","10cm","20cm","30cm"), #Since the soil moisture, soil salinity, soil temperature, and the soil oxygen data are not labelled for the specific datalogger we should change that.
       col = 4:1, 
       pch=1, cex=1)#Since the soil moisture, soil salinity, soil temperature, and the soil oxygen data are not labelled for the specific datalogger we should change that.#Since the soil moisture, soil salinity, soil temperature, and the soil oxygen data are not labelled for the specific datalogger we should change that.

#since we plan to combine the data from all 6 data loggers into one data frame, we should make sure all columns have unique names with the datalogger number
#Since the soil moisture, soil salinity, soil temperature, and the soil oxygen data are not labelled for the specific datalogger we should change that.
#This is big change so we should save the data frame in a different name
CR300_15min_EastDry_021022_V4 <- CR300_15min_EastDry_02022022_V3
colnames(CR300_15min_EastDry_021022_V4)
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture5cm_Avg"] <- "soilmoisture_3_5cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture15cm_Avg"] <- "soilmoisture_3_15cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture25cm_Avg"] <- "soilmoisture_3_25cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture5cm_Std"] <- "soilmoisture_3_5cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture15cm_Std"] <- "soilmoisture_3_15cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture25cm_Std"] <- "soilmoisture_3_25cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture5cm_Max"] <- "soilmoisture_3_5cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture15cm_Max"] <- "soilmoisture_3_15cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture25cm_Max"] <- "soilmoisture_3_25cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture5cm_TMx"] <- "soilmoisture_3_5cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture15cm_TMx"] <- "soilmoisture_3_15cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture25cm_TMx"] <- "soilmoisture_3_25cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture5cm_Min"] <- "soilmoisture_3_5cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture15cm_Min"] <- "soilmoisture_3_15cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture25cm_Min"] <- "soilmoisture_3_25cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture5cm_TMn"] <- "soilmoisture_3_5cm_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture15cm_TMn"] <- "soilmoisture_3_15cm_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilmoisture25cm_TMn"] <- "soilmoisture_3_25cm_TMn"

colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity5cm_Avg"] <- "soilsalinity_3_5cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity15cm_Avg"] <- "soilsalinity_3_15cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity25cm_Avg"] <- "soilsalinity_3_25cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity5cm_Std"] <- "soilsalinity_3_5cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity15cm_Std"] <- "soilsalinity_3_15cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity25cm_Std"] <- "soilsalinity_3_25cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity5cm_Max"] <- "soilsalinity_3_5cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity15cm_Max"] <- "soilsalinity_3_15cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity25cm_Max"] <- "soilsalinity_3_25cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity5cm_TMx"] <- "soilsalinity_3_5cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity15cm_TMx"] <- "soilsalinity_3_15cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity25cm_TMx"] <- "soilsalinity_3_25cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity5cm_Min"] <- "soilsalinity_3_5cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity15cm_Min"] <- "soilsalinity_3_15cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity25cm_Min"] <- "soilsalinity_3_25cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity5cm_TMn"] <- "soilsalinity_3_5cm_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity15cm_TMn"] <- "soilsalinity_3_15cm_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soilsalinity25cm_TMn"] <- "soilsalinity_3_25cm_TMn"

colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature5cm_Avg"] <- "soiltemperature_3_5cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature15cm_Avg"] <- "soiltemperature_3_15cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature25cm_Avg"] <- "soiltemperature_3_25cm_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature5cm_Std"] <- "soiltemperature_3_5cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature15cm_Std"] <- "soiltemperature_3_15cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature25cm_Std"] <- "soiltemperature_3_25cm_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature5cm_Max"] <- "soiltemperature_3_5cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature15cm_Max"] <- "soiltemperature_3_15cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature25cm_Max"] <- "soiltemperature_3_25cm_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature5cm_TMx"] <- "soiltemperature_3_5cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature15cm_TMx"] <- "soiltemperature_3_15cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature25cm_TMx"] <- "soiltemperature_3_25cm_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature5cm_Min"] <- "soiltemperature_3_5cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature15cm_Min"] <- "soiltemperature_3_15cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature25cm_Min"] <- "soiltemperature_3_25cm_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature5cm_TMn"] <- "soiltemperature_3_5cm_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature15cm_TMn"] <- "soiltemperature_3_15cm_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature25cm_TMn"] <- "soiltemperature_3_25cm_TMn"

colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mA_Avg"] <- "soiloxygen_3_mA_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mA_Std"] <- "soiloxygen_3_mA_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mA_Max"] <- "soiloxygen_3_mA_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mA_TMx"] <- "soiloxygen_3_mA_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mA_Min"] <- "soiloxygen_3_mA_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mA_TMn"] <- "soiloxygen_3_mA_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mg_l_Avg"] <- "soiloxygen_3_mg_l_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mg_l_Std"] <- "soiloxygen_3_mg_l_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mg_l_Max"] <- "soiloxygen_3_mg_l_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mg_l_TMx"] <- "soiloxygen_3_mg_l_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mg_l_Min"] <- "soiloxygen_3_mg_l_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiloxygen_mg_l_TMn"] <- "soiloxygen_3_mg_l_TMn"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature_Avg"] <- "soiltemperature_3_Avg"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature_Std"] <- "soiltemperature_3_Std"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature_Max"] <- "soiltemperature_3_Max"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature_TMx"] <- "soiltemperature_3_TMx"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature_Min"] <- "soiltemperature_3_Min"
colnames(CR300_15min_EastDry_021022_V4)[colnames(CR300_15min_EastDry_021022_V4)== "soiltemperature_TMn"] <- "soiltemperature_3_TMn"


# Continuing the data cleaning for the other two redox sensors 2 and 3.................................................................
# Redox sensor 2
CR300_15min_EastDry_021022_V4$Redox_3_2.1_fromtip_Avg_corrected[541:811]
CR300_15min_EastDry_021022_V4$Redox_3_2.2_fromtip_Avg_corrected[541:811]
CR300_15min_EastDry_021022_V4$Redox_3_2.3_fromtip_Avg_corrected[541:811]
CR300_15min_EastDry_021022_V4$Redox_3_2.4_fromtip_Avg_corrected[541:811]

CR300_15min_EastDry_021022_V4$Redox_3_2.4_fromtip_Avg_corrected[541:811] = NA  # This is where you nullify those data points to NA
CR300_15min_EastDry_021022_V4$Redox_3_2.3_fromtip_Avg_corrected[541:811] = NA
CR300_15min_EastDry_021022_V4$Redox_3_2.2_fromtip_Avg_corrected[541:811] = NA
CR300_15min_EastDry_021022_V4$Redox_3_2.1_fromtip_Avg_corrected[541:811] = NA

# plot the corrected data
plot(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_2.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-800, 900))
points(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_2.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_2.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_2.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.35, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)

# doing the same for redox sensor 3
CR300_15min_EastDry_021022_V4$Redox_3_3.1_fromtip_Avg_corrected[541:811]
CR300_15min_EastDry_021022_V4$Redox_3_3.2_fromtip_Avg_corrected[541:811]
CR300_15min_EastDry_021022_V4$Redox_3_3.3_fromtip_Avg_corrected[541:811]
CR300_15min_EastDry_021022_V4$Redox_3_3.4_fromtip_Avg_corrected[541:811]

CR300_15min_EastDry_021022_V4$Redox_3_3.4_fromtip_Avg_corrected[541:811] = NA  # This is where you nullify those data points to NA
CR300_15min_EastDry_021022_V4$Redox_3_3.3_fromtip_Avg_corrected[541:811] = NA
CR300_15min_EastDry_021022_V4$Redox_3_3.2_fromtip_Avg_corrected[541:811] = NA
CR300_15min_EastDry_021022_V4$Redox_3_3.1_fromtip_Avg_corrected[541:811] = NA

# plot the corrected data for redox sensor 3
plot(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_3.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-600, 900))
points(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_3.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_3.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_021022_V4$TIMESTAMP, CR300_15min_EastDry_021022_V4$Redox_3_3.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.35, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)


# Now save again ------------------------------------------------------------------------------This is round 2 of cleaning data
CR300_15min_EastDry_02102022_V5 <- CR300_15min_EastDry_021022_V4

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1589:1652]

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1589:1652] = NA # This is where you nullify those data points to NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1589:1652] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1589:1652] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1589:1652] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1859:1926]

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1859:1926] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1455:1461]

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1455:1461] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1705:1719]

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[1705:1719] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[917:975]

CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected[917:975] = NA

# plot the corrected data for redox sensor 3
plot(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_3.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-600, 800))
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_3.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_3.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_3.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.15, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)

# Now doing the same -----------------------------------------------------------------for redox sensor 2
CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1589:1652]

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1589:1652] = NA # This is where you nullify those data points to NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1589:1652] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1589:1652] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1589:1652] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1859:1926]

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1859:1926] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1455:1461]

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1455:1461] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1705:1719]

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1705:1719] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[917:975]

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[917:975] = NA

plot(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-600, 1000))
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.19, 0), 
       legend = c("above surface","5cm","15cm","25cm"), 
       col = 4:1, 
       pch=1, cex=1)


# Now doing the same -----------------------------------------------------------------for redox sensor 1
CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[1589:1652]
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[1589:1652]

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[1589:1652] = NA # This is where you nullify those data points to NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[1589:1652] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[1589:1652] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[1589:1652] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[1859:1926]
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[1859:1926]

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[1859:1926] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[1859:1926] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[1455:1461]
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[1455:1461]

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[1455:1461] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[1455:1461] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[1705:1719]
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[1705:1719]

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[1705:1719] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[1705:1719] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_2.1_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_2.2_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_2.3_fromtip_Avg_corrected[917:975]
CR300_15min_EastDry_02102022_V5$Redox_3_2.4_fromtip_Avg_corrected[917:975]

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[917:975] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[917:975] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[3061:3128] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[3061:3128] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[3061:3128] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[3061:3128] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[3446] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[3446] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[3446] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[3446] = NA

CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected[5682] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected[5682] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected[5682] = NA
CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected[5682] = NA

plot(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_1.1_fromtip_Avg_corrected, type = 'p', main = "East Dry site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-500, 900))
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_1.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_1.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastDry_02102022_V5$TIMESTAMP, CR300_15min_EastDry_02102022_V5$Redox_3_1.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.12, 0), 
       legend = c("5cm","10cm","20cm","30cm"), #Since the soil moisture, soil salinity, soil temperature, and the soil oxygen data are not labelled for the specific datalogger we should change that.
       col = 4:1, 
       pch=1, cex=1)
