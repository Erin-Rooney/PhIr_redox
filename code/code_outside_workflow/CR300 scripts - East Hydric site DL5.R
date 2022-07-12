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
CR300_15min_cumulative_DL5.East.Hydric_03212022 <- read.csv("C:/Users/savas/OneDrive - University of Tennessee/Desktop/R for PhIr2021/CR300_15min_cumulative_DL5 East Hydric_03212022.csv")

#check the classes of the data
sapply(CR300_15min_cumulative_DL5.East.Hydric_03212022, class)
names(CR300_15min_cumulative_DL5.East.Hydric_03212022)


#All the columns except the dates are being recognized as numeric, we need to convert the characters to integers
# Now duplicate the file from the original file
CR300_15min_EastHydric_03212022_V1 <- CR300_15min_cumulative_DL5.East.Hydric_03212022

#check the classes of the data
sapply(CR300_15min_EastHydric_03212022_V1, class)


# All rows except the timestamps are read as numeric, so nothing but the timestamps need to be changed or read
CR300_15min_EastHydric_03212022_V1$TIMESTAMP <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilmoisture5cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilmoisture5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilmoisture15cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilmoisture15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilmoisture25cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilmoisture25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilmoisture5cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilmoisture5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilmoisture15cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilmoisture15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilmoisture25cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilmoisture25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilsalinity5cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilsalinity5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilsalinity15cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilsalinity15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilsalinity25cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilsalinity25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilsalinity5cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilsalinity5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilsalinity15cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilsalinity15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soilsalinity25cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soilsalinity25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature5cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature15cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature25cm_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature5cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature15cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature25cm_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

#Similarly for redox sensors and the soil oxygen sensors
CR300_15min_EastHydric_03212022_V1$redox_5_1.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.5_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.5_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.5_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.5_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.6_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.6_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.6_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.6_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.7_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.7_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.7_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.7_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.8_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.8_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_1.8_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_1.8_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_EastHydric_03212022_V1$redox_5_2.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5.2.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5.2.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5.2.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5.2.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.5_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.5_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.5_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.5_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.6_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.6_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.6_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.6_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.7_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.7_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.7_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.7_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.8_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.8_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_2.8_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_2.8_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_EastHydric_03212022_V1$redox_5_3.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.5_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.5_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.5_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.5_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.6_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.6_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.6_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.6_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.7_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.7_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.7_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.7_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.8_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.8_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$redox_5_3.8_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$redox_5_3.8_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_EastHydric_03212022_V1$soiloxygen_mg_l_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiloxygen_mg_l_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiloxygen_mg_l_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiloxygen_mg_l_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiloxygen_mA_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiloxygen_mA_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiloxygen_mA_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiloxygen_mA_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature_TMx <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_EastHydric_03212022_V1$soiltemperature_TMn <- as.POSIXct(strptime(CR300_15min_EastHydric_03212022_V1$soiltemperature_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

#check the classes of the data
sapply(CR300_15min_EastHydric_03212022_V1, class)

# Plot 1: Timestamp vs battery voltage, changing axes labels along with it
plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$batt_volt_Min, xlab = "Timestamp", ylab = "Battery Voltage (V)", main = "East Hydric site")

#This is another way to plot the different series onto a single plot. Type 'P' here refers to points. Type 'l' refers to line plot.
plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soilmoisture5cm_Avg, type = 'p', main = "East Hydric site", xlab='Time', ylab='Soil moisture(%)', col='blue', ylim = c(10, 60))
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soilmoisture15cm_Avg, type='p', col='green')
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soilmoisture25cm_Avg, type='p', col='red')
legend('bottomright', legend = c("Above surface", "0-10cm", "10-20cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soiltemperature5cm_Avg, type = 'p', main = "East Hydric site", xlab='Time', ylab='Soil temperature(oC)', col='blue')
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soiltemperature15cm_Avg, type='p', col='green')
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soiltemperature25cm_Avg, type='p', col='red')
legend('topright', legend = c("Above surface", "0-10cm", "10-20cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soilsalinity5cm_Avg, type = 'p', main = "East Hydric site", xlab='Time', ylab='Soil salinity(VIC)', col='blue', ylim = c(800, 3000))
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soilsalinity15cm_Avg, type='p', col='green')
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soilsalinity25cm_Avg, type='p', col='red')
legend('topright', legend = c("Above surface", "0-10cm", "10-20cm"), col = c("Blue","Green", "Red"), pch=1, cex=1)

plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$soiloxygen_mg_l_Avg, type = 'p', main = "East Hydric site", xlab='Time', ylab='Soil oxygen (mg/L)', col='blue', ylim = c(0, 6))
legend('topleft', legend = c("10cm"), col = c("Blue"), pch=1, cex=1)


# For the redox data. We will need to correct them for standard hydrogen electrode. This is done by adding new columns that calurate the corrected redox data
# Using average soil temperature values, Above surface = 9 C, 0-10cm = 4 C, and 10-20cm = 2 C. For all depths below 30cm we will use the same temperatures.
#Since saturated KCl was used this sawyers formula is used to correct the redox potential to SHE electrode from Ag/AgCl electrode, E = 199 - 1.01*(T-25C)
names(CR300_15min_EastHydric_03212022_V1)

CR300_15min_EastHydric_03212022_V1$Redox_5_1.1_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,60]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_1.2_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,61]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_1.3_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,62]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_1.4_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,63]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_1.5_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,65]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_1.6_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,66]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_1.7_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,67]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_1.8_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,68]+222

CR300_15min_EastHydric_03212022_V1$Redox_5_2.1_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,114]+215
CR300_15min_EastHydric_03212022_V1$Redox_5_2.2_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,115]+215
CR300_15min_EastHydric_03212022_V1$Redox_5_2.3_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,116]+215
CR300_15min_EastHydric_03212022_V1$Redox_5_2.4_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,117]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_2.5_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,119]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_2.6_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,120]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_2.7_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,121]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_2.8_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,122]+222

CR300_15min_EastHydric_03212022_V1$Redox_5_3.1_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,168]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_3.2_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,169]+220
CR300_15min_EastHydric_03212022_V1$Redox_5_3.3_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,170]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_3.4_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,171]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_3.5_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,173]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_3.6_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,174]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_3.7_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,175]+222
CR300_15min_EastHydric_03212022_V1$Redox_5_3.8_fromtip_Avg_corrected <- CR300_15min_EastHydric_03212022_V1[,176]+222

# To delete a column in the data frame. I am using this because when I was creating the corrected columns for the redox data, I accidentally missed 5_2.8 redox probe which confuses everything.
CR300_15min_EastHydric_03212022_V1$Redox_5_3.8_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_3.7_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_3.6_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_3.5_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_3.4_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_3.3_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_3.2_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_3.1_fromtip_Avg_corrected = NULL
CR300_15min_EastHydric_03212022_V1$Redox_5_2.8_fromtip_Avg_corrected = NULL

# To check if the values were calculated correctly using the correct column values
CR300_15min_EastHydric_03212022_V1[, 246]
CR300_15min_EastHydric_03212022_V1[, 247]
CR300_15min_EastHydric_03212022_V1[, 248]
CR300_15min_EastHydric_03212022_V1[, 249]
CR300_15min_EastHydric_03212022_V1[, 250]
CR300_15min_EastHydric_03212022_V1[, 251]
CR300_15min_EastHydric_03212022_V1[, 252]
CR300_15min_EastHydric_03212022_V1[, 253]

CR300_15min_EastHydric_03212022_V1[, 254]
CR300_15min_EastHydric_03212022_V1[, 255]
CR300_15min_EastHydric_03212022_V1[, 256]
CR300_15min_EastHydric_03212022_V1[, 257]
CR300_15min_EastHydric_03212022_V1[, 258]
CR300_15min_EastHydric_03212022_V1[, 259]
CR300_15min_EastHydric_03212022_V1[, 260]
CR300_15min_EastHydric_03212022_V1[, 261]

CR300_15min_EastHydric_03212022_V1[, 262]
CR300_15min_EastHydric_03212022_V1[, 263]
CR300_15min_EastHydric_03212022_V1[, 264]
CR300_15min_EastHydric_03212022_V1[, 265]
CR300_15min_EastHydric_03212022_V1[, 266]
CR300_15min_EastHydric_03212022_V1[, 267]
CR300_15min_EastHydric_03212022_V1[, 268]
CR300_15min_EastHydric_03212022_V1[, 269]

sapply(CR300_15min_EastHydric_03212022_V1, class) # Check if they were deleted


# Now that I have all the redox data corrected, I need to plot to see how the data looks. 
par(mar = c(5, 4, 4, 9),
    xpd = TRUE)           # Drawing margin outside the graph area for the legend

plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.8_fromtip_Avg_corrected, type = 'p', main = "East Hydric site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-1500, 1100))
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_1.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.20, 0), 
       legend = c("1cm","3cm","5cm","7cm","9cm","19cm","29cm","49cm"), 
       col = 1:8, 
       pch=1, cex=1)

plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.8_fromtip_Avg_corrected, type = 'p', main = "East Hydric site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-200, 900))
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_2.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.35, 0), 
       legend = c("Above surface 1","Above surface 2","Above surface 3","1cm","3cm","13cm","23cm","43cm"), 
       col = 1:8, 
       pch=1, cex=1)

plot(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.8_fromtip_Avg_corrected, type = 'p', main = "East Hydric site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-400, 800))
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.7_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.6_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.5_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.4_fromtip_Avg_corrected, type='p', col=5, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.3_fromtip_Avg_corrected, type='p', col=6, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.2_fromtip_Avg_corrected, type='p', col=7, cex = 0.5)
points(CR300_15min_EastHydric_03212022_V1$TIMESTAMP, CR300_15min_EastHydric_03212022_V1$Redox_5_3.1_fromtip_Avg_corrected, type='p', col=8, cex = 0.5)

legend("topright", inset = c(-0.2, 0), 
       legend = c("7cm","9cm","11cm","13cm","15cm","24cm","35cm","55cm"), 
       col = 1:8, 
       pch=1, cex=1)


# Now we have all the data plotted, but we need to start cleaning the data
#Step 1: Remove unnecessary columns in the data frame
# Remember you need to save this as a new data frame just so you can trace back your steps
CR300_15min_EastHydric_03212022_V2 <- CR300_15min_EastHydric_03212022_V1
names(CR300_15min_EastHydric_03212022_V2)

CR300_15min_EastHydric_03212022_V2[64]<-NULL
CR300_15min_EastHydric_03212022_V2[72]<-NULL #Since column numbers were used here, be careful that the col numbers obviously shift if a column is deleted. Therefore these decresed numbers in brackets incorporate those changes.
CR300_15min_EastHydric_03212022_V2[80]<-NULL # Above comment applies for all the commands from here and below.  
CR300_15min_EastHydric_03212022_V2[88]<-NULL
CR300_15min_EastHydric_03212022_V2[96]<-NULL
CR300_15min_EastHydric_03212022_V2[104]<-NULL
CR300_15min_EastHydric_03212022_V2[112]<-NULL
CR300_15min_EastHydric_03212022_V2[120]<-NULL
CR300_15min_EastHydric_03212022_V2[128]<-NULL
CR300_15min_EastHydric_03212022_V2[136]<-NULL
CR300_15min_EastHydric_03212022_V2[144]<-NULL
CR300_15min_EastHydric_03212022_V2[152]<-NULL
CR300_15min_EastHydric_03212022_V2[160]<-NULL
CR300_15min_EastHydric_03212022_V2[168]<-NULL
CR300_15min_EastHydric_03212022_V2[176]<-NULL
CR300_15min_EastHydric_03212022_V2[184]<-NULL
CR300_15min_EastHydric_03212022_V2[192]<-NULL
CR300_15min_EastHydric_03212022_V2[200]<-NULL
CR300_15min_EastHydric_03212022_V2[204:209]<-NULL

names(CR300_15min_EastHydric_03212022_V2) # Check if they were deleted


#Next round of data cleaning..........save again here.......This for the redox sensor 3
CR300_15min_EastHydric_03212022_V3 <- CR300_15min_EastHydric_03212022_V2

