# Worksheet 18

data <- read.csv("LowRepeated.csv")

# since plot is not useful, let us remove it from the data
data <- data[-3,]
data <- data[-2,]
data <- data.frame(data)
types <- data[3,]
types
times <- data[1,]
times
# There is bias in the data => find it out and analyse accordingly
# Is time the bias?

# Let us split the data based on time and then analyse
par(mfrow = c(1,5))
plot(data[,1], data[,2], type = "l", title = "Wavelengths vs Readings at 9:14 am", xlab = "wavelength", ylab = "spectral reflectance")
lines(data[,1], data[,3])
lines(data[,1], data[,4])
lines(data[,1], data[,5])
lines(data[,1], data[,6])
lines(data[,1], data[,7])
lines(data[,1], data[,8])
lines(data[,1], data[,9])
# best wavelength for differentiating is between 600 and 650

plot(data[,1], data[,10], type = "l", title = "Wavelengths vs Readings at 10:00 am", xlab = "wavelength", ylab = "spectral reflectance")
lines(data[,1], data[,11])
lines(data[,1], data[,12])
lines(data[,1], data[,13])
lines(data[,1], data[,14])
lines(data[,1], data[,15])
lines(data[,1], data[,16])
lines(data[,1], data[,17])
# best wavelength for differentiating is around 800

plot(data[,1], data[,18], type = "l", title = "Wavelengths vs Readings at 1:30 pm", xlab = "wavelength", ylab = "spectral reflectance")
lines(data[,1], data[,19])
lines(data[,1], data[,20])
lines(data[,1], data[,21])
lines(data[,1], data[,22])
lines(data[,1], data[,23])
lines(data[,1], data[,24])
lines(data[,1], data[,25])
# best wavelength for differentiating is between 625 and 650

plot(data[,1], data[,26], type = "l", title = "Wavelengths vs Readings at 2:20 pm", xlab = "wavelength", ylab = "spectral reflectance")
lines(data[,1], data[,27])
lines(data[,1], data[,28])
lines(data[,1], data[,29])
lines(data[,1], data[,30])
lines(data[,1], data[,31])
lines(data[,1], data[,32])
lines(data[,1], data[,33])
# best wavelength for differentiating is around 550

# From the graphs, we can see that over different times, the patterns vary
# The best wavelength for differentiating the plots will vary based on the time of the day!

# Now what if we don't split by time?
data
# Let us merge this data over time periods by taking averages and then plot the same graphs
newdata <- data[,1:9]
for (j in 3:1540){
  for (i in 2:9){
    newdata[j,i] <- (as.numeric(data[j,i])+as.numeric(data[j,i+8])+as.numeric(data[j,i+16])+as.numeric(data[j,i+24]))/4
  }
}
newdata
plot(newdata[,1], newdata[,2], type = "l",, xlab = "wavelength", ylab = "spectral reflectance")
lines(newdata[,1], newdata[,3])
lines(newdata[,1], newdata[,4])
lines(newdata[,1], newdata[,5])
lines(newdata[,1], newdata[,6])
lines(newdata[,1], newdata[,7])
lines(newdata[,1], newdata[,8])
lines(newdata[,1], newdata[,9])
# best wavelength at measuring is around 780, from the consolidated data, but this is not same if we split it time wise. This is the bias in the data.