#To know which directory you are currently in
getwd()

#TO change the directory
setwd()
#To change the directory path one must have the new path handy
#Make sure to change all the backward slashes '\' to forward slashes '/' 
setwd('C:/Users/koolb/Desktop/R for PhIr2021')

# Create a new project with this directory and R will start reading all the files in that folder

# To read a .dat or .csv file in that folder use the 'import dataset' function under file
#01122022 Since we had the data in two different files, they were consolidated to another file for CR1000_15min_cumulative_01122022
CR1000_15min_cumulative_01122022 <- read.csv("C:/Users/savas/OneDrive - University of Tennessee/Desktop/R for PhIr2021/CR1000_15min_cumulative_01122022.csv", header=TRUE)

#check the classes of the data
sapply(CR1000_15min_cumulative_01122022, class)


#All the columns except the dates are being recognized as numeric, we need to convert the characters to integers
# Now duplicate the file from the original file
CR1000_15min_cumulative_01122022_V1 <- CR1000_15min_cumulative_01122022

#check the classes of the data
sapply(CR1000_15min_cumulative_01122022_V1, class)


# All rows except the timestamps are read as numeric, so nothing but the timestamps need to be changed or read
CR1000_15min_cumulative_01122022_V1$TIMESTAMP <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilmoisture5cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilmoisture5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilmoisture15cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilmoisture15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilmoisture25cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilmoisture25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilmoisture5cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilmoisture5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilmoisture15cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilmoisture15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilmoisture25cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilmoisture25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilsalinity5cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilsalinity5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilsalinity15cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilsalinity15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilsalinity25cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilsalinity25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilsalinity5cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilsalinity5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilsalinity15cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilsalinity15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soilsalinity25cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soilsalinity25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiltemperature5cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiltemperature5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiltemperature15cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiltemperature15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiltemperature25cm_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiltemperature25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiltemperature5cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiltemperature5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiltemperature15cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiltemperature15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiltemperature25cm_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiltemperature25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

#Similarly for redox sensors and the soil oxygen sensors
CR1000_15min_cumulative_01122022_V1$Redox_6_1.1_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.1_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.2_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.2_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.3_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.3_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.4_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.4_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.5_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.5_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.5_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.5_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.6_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.6_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.6_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.6_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.7_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.7_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.7_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.7_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.8_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.8_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_1.8_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_1.8_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR1000_15min_cumulative_01122022_V1$Redox_6_2.1_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.1_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.2_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.2_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.3_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.3_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.4_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.4_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.5_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.5_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.5_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.5_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.6_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.6_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.6_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.6_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.7_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.7_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.7_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.7_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.8_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.8_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_2.8_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_2.8_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR1000_15min_cumulative_01122022_V1$Redox_6_3.1_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.1_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.2_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.2_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.3_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.3_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.4_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.4_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.5_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.5_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.5_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.5_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.6_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.6_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.6_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.6_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.7_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.7_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.7_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.7_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.8_fromtip_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.8_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$Redox_6_3.8_fromtip_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$Redox_6_3.8_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR1000_15min_cumulative_01122022_V1$soiloxygen_mg_l_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiloxygen_mg_l_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiloxygen_mg_l_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiloxygen_mg_l_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiloxygen_mV_TMx <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiloxygen_mV_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR1000_15min_cumulative_01122022_V1$soiloxygen_mV_TMn <- as.POSIXct(strptime(CR1000_15min_cumulative_01122022_V1$soiloxygen_mV_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

#check the classes of the data
sapply(CR1000_15min_cumulative_01122022_V1, class)


#This is another way to plot the different series onto a single plot. Type 'P' here refers to points. Type 'l' refers to line plot.
plot(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soilmoisture5cm_Avg, type = 'p', main = "West Hydric site", xlab='Time', ylab='Soil moisture(%)', col='blue', ylim = c(10, 60))
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soilmoisture15cm_Avg, type='p', col='green')
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soilmoisture25cm_Avg, type='p', col='red')
legend('bottomright', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soiltemperature5cm_Avg, type = 'p', main = "West Hydric site", xlab='Time', ylab='Soil temperature(oC)', col='blue')
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soiltemperature15cm_Avg, type='p', col='green')
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soiltemperature25cm_Avg, type='p', col='red')
legend('topright', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soilsalinity5cm_Avg, type = 'p', main = "West Hydric site", xlab='Time', ylab='Soil salinity(VIC)', col='blue')
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soilsalinity15cm_Avg, type='p', col='green')
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soilsalinity25cm_Avg, type='p', col='red')
legend('topleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$soiloxygen_mg_l_Avg, type = 'p', main = "West Hydric site", xlab='Time', ylab='Soil oxygen (mg/L)', col='blue')
legend('topleft', legend = c("10cm"), col = c("Blue"), pch=1, cex=1)


# For the redox data. We will need to correct them for standard hydrogen electrode. This is done by adding new columns that calurate the corrected redox data
CR1000_15min_cumulative_01122022_V1$Redox_6_1.8_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,85]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_1.7_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,86]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_1.6_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,87]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_1.5_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,88]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_1.4_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,89]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_1.3_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,90]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_1.2_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,91]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_1.1_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,92]+218

CR1000_15min_cumulative_01122022_V1$Redox_6_2.8_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,94]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_2.7_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,95]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_2.6_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,96]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_2.5_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,97]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_2.4_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,98]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_2.3_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,99]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_2.2_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,100]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_2.1_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022[,101]+218

CR1000_15min_cumulative_01122022_V1$Redox_6_3.8_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,102]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_3.7_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,103]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_3.6_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,104]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_3.5_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,105]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_3.4_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,106]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_3.3_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,107]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_3.2_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,108]+218
CR1000_15min_cumulative_01122022_V1$Redox_6_3.1_fromtip_Avg_corrected <- CR1000_15min_cumulative_01122022_V1[,109]+218

# To check if the values were calculated correctly using the correct column values
CR1000_15min_cumulative_01122022_V1[, 247]
CR1000_15min_cumulative_01122022_V1[, 248]
CR1000_15min_cumulative_01122022_V1[, 249]
CR1000_15min_cumulative_01122022_V1[, 250]
CR1000_15min_cumulative_01122022_V1[, 251]
CR1000_15min_cumulative_01122022_V1[, 252]
CR1000_15min_cumulative_01122022_V1[, 253]
CR1000_15min_cumulative_01122022_V1[, 254]

CR1000_15min_cumulative_01122022_V1[, 255]
CR1000_15min_cumulative_01122022_V1[, 256]
CR1000_15min_cumulative_01122022_V1[, 257]
CR1000_15min_cumulative_01122022_V1[, 258]
CR1000_15min_cumulative_01122022_V1[, 259]
CR1000_15min_cumulative_01122022_V1[, 260]
CR1000_15min_cumulative_01122022_V1[, 261]
CR1000_15min_cumulative_01122022_V1[, 262]

CR1000_15min_cumulative_01122022_V1[, 263]
CR1000_15min_cumulative_01122022_V1[, 264]
CR1000_15min_cumulative_01122022_V1[, 265]
CR1000_15min_cumulative_01122022_V1[, 266]
CR1000_15min_cumulative_01122022_V1[, 267]
CR1000_15min_cumulative_01122022_V1[, 268]
CR1000_15min_cumulative_01122022_V1[, 269]
CR1000_15min_cumulative_01122022_V1[, 270]

# To delete a column in the data frame. I am using this because when I was creating the corrected columns for the redox data, I accidentally missed 6_2.1 redox probe which confuses everything.
CR1000_15min_cumulative_01122022_V1$Redox_6_3.8_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_3.7_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_3.6_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_3.5_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_3.4_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_3.3_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_3.2_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_3.1_fromtip_Avg_corrected = NULL

CR1000_15min_cumulative_01122022_V1$Redox_6_2.8_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_2.7_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_2.6_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_2.5_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_2.4_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_2.3_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_2.2_fromtip_Avg_corrected = NULL
CR1000_15min_cumulative_01122022_V1$Redox_6_2.1_fromtip_Avg_corrected = NULL

sapply(CR1000_15min_cumulative_01122022_V1, class) # Check if they were deleted


# Now that I have all the redox data corrected, I need to plot to see how the data looks. 
par(mar = c(5, 4, 4, 9),
    xpd = TRUE)           # Drawing margin outside the graph area for the legend

plot(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.8_fromtip_Avg_corrected, type = 'p', main = "West Hydric site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-200, 1100))
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_1.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.20, 0), 
       legend = c("5cm","7cm","9cm","11cm","13cm","23cm","33cm","53cm"), 
       col = 1:8, 
       pch=1, cex=1.5)

plot(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.8_fromtip_Avg_corrected, type = 'p', main = "West Hydric site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-200, 1100))
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_2.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.2, 0), 
       legend = c("5cm","7cm","9cm","11cm","13cm","23cm","33cm","53cm"), 
       col = 1:8, 
       pch=1, cex=1.5)

plot(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.8_fromtip_Avg_corrected, type = 'p', main = "West Hydric site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-200, 1100))
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V1$TIMESTAMP, CR1000_15min_cumulative_01122022_V1$Redox_6_3.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.2, 0), 
       legend = c("5cm","7cm","9cm","11cm","13cm","23cm","33cm","53cm"), 
       col = 1:8, 
       pch=1, cex=1.5)


# Now we have all the data plotted, but we need to start cleaning the data
#Step 1: Remove unnecessary columns in the data frame
# Remember you need to save this as a new data frame just so you can trace back your steps
CR1000_15min_cumulative_01122022_V2 <- CR1000_15min_cumulative_01122022_V1

names(CR1000_15min_cumulative_01122022_V2[68]) #Just checking to make sure I am deleting the right columns
names(CR1000_15min_cumulative_01122022_V2[93])
names(CR1000_15min_cumulative_01122022_V2[118])
names(CR1000_15min_cumulative_01122022_V2[143])
names(CR1000_15min_cumulative_01122022_V2[168])
names(CR1000_15min_cumulative_01122022_V2[193])
names(CR1000_15min_cumulative_01122022_V2[218])
CR1000_15min_cumulative_01122022_V2[68]
names(CR1000_15min_cumulative_01122022_V2)

CR1000_15min_cumulative_01122022_V2[68]<-NULL
CR1000_15min_cumulative_01122022_V2[92]<-NULL #Since column numbers were used here, be careful that the col numbers obviously shift if a column is deleted. Therefore these decresed numbers in brackets incorporate those changes.
CR1000_15min_cumulative_01122022_V2[116]<-NULL # Above comment applies for all the commands from here and below.  
CR1000_15min_cumulative_01122022_V2[140]<-NULL
CR1000_15min_cumulative_01122022_V2[164]<-NULL
CR1000_15min_cumulative_01122022_V2[188]<-NULL
CR1000_15min_cumulative_01122022_V2[212]<-NULL

names(CR1000_15min_cumulative_01122022_V2) # Check if they were deleted


#Next round of data cleaning..........save again here.......This for the redox sensor 3
CR1000_15min_cumulative_01122022_V3 <- CR1000_15min_cumulative_01122022_V2

CR1000_15min_cumulative_01122022_V3$Redox_6_3.8_fromtip_Avg_corrected[1081:1091]
CR1000_15min_cumulative_01122022_V3$Redox_6_3.7_fromtip_Avg_corrected[1081:1091]
CR1000_15min_cumulative_01122022_V3$Redox_6_3.6_fromtip_Avg_corrected[1081:1091]
CR1000_15min_cumulative_01122022_V3$Redox_6_3.5_fromtip_Avg_corrected[1081:1091]
CR1000_15min_cumulative_01122022_V3$Redox_6_3.4_fromtip_Avg_corrected[1081:1091]
CR1000_15min_cumulative_01122022_V3$Redox_6_3.3_fromtip_Avg_corrected[1081:1091]
CR1000_15min_cumulative_01122022_V3$Redox_6_3.2_fromtip_Avg_corrected[1081:1091]
CR1000_15min_cumulative_01122022_V3$Redox_6_3.1_fromtip_Avg_corrected[1081:1091]

CR1000_15min_cumulative_01122022_V3$Redox_6_3.8_fromtip_Avg_corrected[1081:1091] = NA #Chang the values to NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.7_fromtip_Avg_corrected[1081:1091] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.6_fromtip_Avg_corrected[1081:1091] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.5_fromtip_Avg_corrected[1081:1091] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.4_fromtip_Avg_corrected[1081:1091] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.3_fromtip_Avg_corrected[1081:1091] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.2_fromtip_Avg_corrected[1081:1091] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.1_fromtip_Avg_corrected[1081:1091] = NA

CR1000_15min_cumulative_01122022_V3$Redox_6_3.8_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.7_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.6_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.5_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.4_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.3_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.2_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.1_fromtip_Avg_corrected[431:437] = NA

CR1000_15min_cumulative_01122022_V3$Redox_6_3.8_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.7_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.6_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.5_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.4_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.3_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.2_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V3$Redox_6_3.1_fromtip_Avg_corrected[1446:1448] = NA

# Not sure about this change, so saving here again..........redox sensor 3
CR1000_15min_cumulative_01122022_V4 <- CR1000_15min_cumulative_01122022_V3

CR1000_15min_cumulative_01122022_V4$Redox_6_3.8_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.7_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.6_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.5_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.4_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.3_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.2_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.1_fromtip_Avg_corrected[1079:1152] = NA

CR1000_15min_cumulative_01122022_V4$Redox_6_3.8_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.7_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.6_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.5_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.4_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.3_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.2_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_3.1_fromtip_Avg_corrected[5111:5112] = NA


par(mar = c(5, 4, 4, 7),
    xpd = TRUE) 

plot(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.8_fromtip_Avg_corrected, type = 'p', main = "West Hydric site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.8, ylim = c(-200, 800))
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_3.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.16, 0), 
       legend = c("5cm","7cm","9cm","11cm","13cm","23cm","33cm","53cm"), 
       col = 1:8, 
       pch=1, cex=1.2)


# Now cleaning redox sensor 2 data......................................................................................................
# The cleaning is mostly necessary at the same points so it will be easy to just copy and make the necessary changes

par(mar = c(5, 4, 4, 7),
    xpd = TRUE) 

plot(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.8_fromtip_Avg_corrected, type = 'p', main = "West Hydric site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-200, 800))
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_2.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.16, 0), 
       legend = c("5cm","7cm","9cm","11cm","13cm","23cm","33cm","53cm"), 
       col = 1:8, 
       pch=1, cex=1.5)

CR1000_15min_cumulative_01122022_V4$Redox_6_2.8_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.7_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.6_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.5_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.4_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.3_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.2_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.1_fromtip_Avg_corrected[1079:1152] = NA

CR1000_15min_cumulative_01122022_V4$Redox_6_2.8_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.7_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.6_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.5_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.4_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.3_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.2_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.1_fromtip_Avg_corrected[431:437] = NA

CR1000_15min_cumulative_01122022_V4$Redox_6_2.8_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.7_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.6_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.5_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.4_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.3_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.2_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.1_fromtip_Avg_corrected[1446:1448] = NA

CR1000_15min_cumulative_01122022_V4$Redox_6_2.8_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.7_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.6_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.5_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.4_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.3_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.2_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_2.1_fromtip_Avg_corrected[5111:5112] = NA


# Now cleaning redox sensor 1 data......................................................................................................
# The cleaning is mostly necessary at the same points so it will be easy to just copy and make the necessary changes

par(mar = c(5, 4, 4, 9),
    xpd = TRUE) 

plot(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.8_fromtip_Avg_corrected, type = 'p', main = "West Hydric site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-200, 800))
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR1000_15min_cumulative_01122022_V4$TIMESTAMP, CR1000_15min_cumulative_01122022_V4$Redox_6_1.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.2, 0), 
       legend = c("5cm","7cm","9cm","11cm","13cm","23cm","33cm","53cm"), 
       col = 1:8, 
       pch=1, cex=1.5)

CR1000_15min_cumulative_01122022_V4$Redox_6_1.8_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.7_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.6_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.5_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.4_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.3_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.2_fromtip_Avg_corrected[1079:1152] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.1_fromtip_Avg_corrected[1079:1152] = NA

CR1000_15min_cumulative_01122022_V4$Redox_6_1.8_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.7_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.6_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.5_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.4_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.3_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.2_fromtip_Avg_corrected[431:437] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.1_fromtip_Avg_corrected[431:437] = NA

CR1000_15min_cumulative_01122022_V4$Redox_6_1.8_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.7_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.6_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.5_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.4_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.3_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.2_fromtip_Avg_corrected[1446:1448] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.1_fromtip_Avg_corrected[1446:1448] = NA

CR1000_15min_cumulative_01122022_V4$Redox_6_1.8_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.7_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.6_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.5_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.4_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.3_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.2_fromtip_Avg_corrected[5111:5112] = NA
CR1000_15min_cumulative_01122022_V4$Redox_6_1.1_fromtip_Avg_corrected[5111:5112] = NA

