# To read a .dat or .csv file in that folder use the 'import dataset' function under file
# Below is an example to open the CR300 csv file in that directory. 
CR300_15min_cumulative_DL2.West.Mesic_03182022 <- read.csv("C:/Users/savas/OneDrive - University of Tennessee/Desktop/R for PhIr2021/CR300_15min_cumulative_DL2 West Mesic_03182022.csv")

CR300_15min_WestMesic_03212022_V1 <- CR300_15min_cumulative_DL2.West.Mesic_03182022 # Creating a duplicate of an existing raw data file
# Instead of doing the above step just create a copy of the file in the folder itself and edit the file to ensure that the first line is the header.
# Then read that file in R including a command that the first line is the header.

#check the classes of the data
sapply(CR300_15min_WestMesic_03212022_V1, class)

#Since all of our data is in one file nothing needs to be changed or organized in the data before moving forward to the next step
#Most of the columns except the date columns and few have been recognized as numbers. So that needs to be changed. Below is a command for reading dates
CR300_15min_WestMesic_03212022_V1$TIMESTAMP <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilmoisture5cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilmoisture5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilmoisture15cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilmoisture15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilmoisture25cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilmoisture25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilmoisture5cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilmoisture5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilmoisture15cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilmoisture15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilmoisture25cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilmoisture25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilsalinity5cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilsalinity5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilsalinity15cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilsalinity15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilsalinity25cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilsalinity25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilsalinity5cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilsalinity5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilsalinity15cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilsalinity15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soilsalinity25cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soilsalinity25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature5cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature5cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature15cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature15cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature25cm_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature25cm_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature5cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature5cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature15cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature15cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature25cm_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature25cm_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

#Similarly for redox sensors and the soil oxygen sensors
CR300_15min_WestMesic_03212022_V1$redox_2_1.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_1.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_1.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_1.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_1.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_1.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_1.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_1.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_1.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_WestMesic_03212022_V1$redox_2_2.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_2.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_2.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_2.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_2.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_2.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_2.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_2_2.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_2.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))

CR300_15min_WestMesic_03212022_V1$redox_3_3.1_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.1_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_3_3.1_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.1_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_3_3.2_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.2_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_3_3.2_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.2_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_3_3.3_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.3_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_3_3.3_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.3_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_3_3.4_fromtip_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.4_fromtip_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$redox_3_3.4_fromtip_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$redox_2_3.4_fromtip_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))


CR300_15min_WestMesic_03212022_V1$soiloxygen_mg_l_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiloxygen_mg_l_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiloxygen_mg_l_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiloxygen_mg_l_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiloxygen_mA_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiloxygen_mA_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiloxygen_mA_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiloxygen_mA_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature_TMx <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature_TMx, "%Y-%m-%d %H:%M:%S", tz="GMT"))
CR300_15min_WestMesic_03212022_V1$soiltemperature_TMn <- as.POSIXct(strptime(CR300_15min_WestMesic_03212022_V1$soiltemperature_TMn, "%Y-%m-%d %H:%M:%S", tz="GMT"))


# I have converted all colummns to numerical and date formats. Now I need to be able to plot the data

# Plot 1: Timestamp vs battery voltage, changing axes labels along with it
plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$batt_volt_Min, xlab = "Timestamp", ylab = "Battery Voltage (V)", main = "West Mesic site")

#plot 2
#This is one way to plot the different series onto a single plot. Type 'P' here refers to points. Type 'l' refers to line plot.
plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soilmoisture5cm_Avg, type = 'p', main = "West Mesic site", xlab='Time', ylab='Soil moisture(%)', col='blue', ylim = c(-20, 60))
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soilmoisture15cm_Avg, type='p', col='green')
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soilmoisture25cm_Avg, type='p', col='red')
legend('bottomleft', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.82)

plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soiltemperature5cm_Avg, type = 'p', main = "West Mesic site", xlab='Time', ylab='Soil temperature(oC)', col='blue')
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soiltemperature15cm_Avg, type='p', col='green')
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soiltemperature25cm_Avg, type='p', col='red')
legend('topright', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.68)

plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soilsalinity5cm_Avg, type = 'p', main = "West Mesic site", xlab='Time', ylab='Soil salinity(VIC)', col='blue', ylim = c(600, 1500))
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soilsalinity15cm_Avg, type='p', col='green')
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soilsalinity25cm_Avg, type='p', col='red')
legend('bottomright', legend = c("0-10cm", "10-20cm", "20-30cm"), col = c("Blue","Green", "Red"), pch=1, cex=0.85)

plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$soiloxygen_mg_l_Avg, type = 'p', main = "West Mesic site", xlab='Time', ylab='Soil oxygen (mg/L)', col='blue')
legend('bottomright', legend = c("10cm"), col = c("Blue"), pch=1, cex=1)


# For the redox data. We will need to correct them for standard hydrogen electrode. This is done by adding new columns that calculate the corrected redox data. 
# Fist use names to identify the column numbers 
names(CR300_15min_WestMesic_03212022_V1)

#Use the accurate column numbers to calculate this new column.
# Using average soil temperature values, 0-10cm = 7 C, 10-20cm = 2 C, and 20-30cm = 0 C. 
#Since saturated KCl was used this sawyers formula is used to correct the redox potential to SHE electrode from Ag/AgCl electrode, E = 199 - 1.01*(T-25C)
CR300_15min_WestMesic_03212022_V1$Redox_2_1.1_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,60]+217
CR300_15min_WestMesic_03212022_V1$Redox_2_1.2_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,61]+217
CR300_15min_WestMesic_03212022_V1$Redox_2_1.3_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,62]+222
CR300_15min_WestMesic_03212022_V1$Redox_2_1.4_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,63]+224

CR300_15min_WestMesic_03212022_V1$Redox_2_2.1_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,84]+217
CR300_15min_WestMesic_03212022_V1$Redox_2_2.2_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,85]+217
CR300_15min_WestMesic_03212022_V1$Redox_2_2.3_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,86]+222
CR300_15min_WestMesic_03212022_V1$Redox_2_2.4_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,87]+224

CR300_15min_WestMesic_03212022_V1$Redox_2_3.1_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,108]+217
CR300_15min_WestMesic_03212022_V1$Redox_2_3.2_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,109]+217
CR300_15min_WestMesic_03212022_V1$Redox_2_3.3_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,110]+222
CR300_15min_WestMesic_03212022_V1$Redox_2_3.4_fromtip_Avg_corrected <- CR300_15min_WestMesic_03212022_V1[,111]+224

# To check if the values were calculated correctly using the correct column values
CR300_15min_WestMesic_03212022_V1[, 164]
CR300_15min_WestMesic_03212022_V1[, 165]
CR300_15min_WestMesic_03212022_V1[, 166]
CR300_15min_WestMesic_03212022_V1[, 167]

CR300_15min_WestMesic_03212022_V1[, 168]
CR300_15min_WestMesic_03212022_V1[, 169]
CR300_15min_WestMesic_03212022_V1[, 170]
CR300_15min_WestMesic_03212022_V1[, 171]

CR300_15min_WestMesic_03212022_V1[, 172]
CR300_15min_WestMesic_03212022_V1[, 173]
CR300_15min_WestMesic_03212022_V1[, 174]
CR300_15min_WestMesic_03212022_V1[, 175]


# Now that I have all the redox data corrected, I need to plot to see how the data looks. 
par(mar = c(5, 4, 4, 6),
    xpd = TRUE)           # Drawing margin outside the graph area for the legend

plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_1.1_fromtip_Avg_corrected, type = 'p', main = "West Mesic site plot-1", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-2000, 1100))
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_1.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_1.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_1.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.2, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)

plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_2.1_fromtip_Avg_corrected, type = 'p', main = "West Mesic site plot-2", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-2000, 1100))
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_2.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_2.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_2.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.2, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=0.8)

plot(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_3.1_fromtip_Avg_corrected, type = 'p', main = "West Mesic site plot-3", xlab='Time', ylab='Redox potential (mv)', col=1, cex = 0.5, ylim = c(-2000, 1100))
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_3.2_fromtip_Avg_corrected, type='p', col=2, cex = 0.5)
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_3.3_fromtip_Avg_corrected, type='p', col=3, cex = 0.5)
points(CR300_15min_WestMesic_03212022_V1$TIMESTAMP, CR300_15min_WestMesic_03212022_V1$Redox_2_3.4_fromtip_Avg_corrected, type='p', col=4, cex = 0.5)

legend("topright", inset = c(-0.2, 0), 
       legend = c("5cm","10cm","20cm","30cm"), 
       col = 4:1, 
       pch=1, cex=1)


# Fist use names to identify the column numbers 
names(CR300_15min_WestMesic_03212022_V1)

# Removing the ref columns as those data sets mean nothing.I saved this as another version. CR300_15min_EastDry_02012022_V1
CR300_15min_WestMesic_03212022_V2 <- CR300_15min_WestMesic_03212022_V1
CR300_15min_WestMesic_03212022_V2[132:137]<-NULL
names(CR300_15min_WestMesic_03212022_V2)

#Data cleaning for redox data. Removing the outliers. I created another copy of the file here. CR300_15min_EastDry_02022022_V2
#Step 1 create a new version of the data frame
CR300_15min_WestMesic_03212022_V3 <- CR300_15min_WestMesic_03212022_V2

