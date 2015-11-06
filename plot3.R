# Reading and filtering power data placed on its own function,
#   so that large intermediate variables are automatically released
#
# This function reads all data and produces a data frame used in the plots
#
# The produced data frame has an additional column - DateTime - 
#   which combines "Date" and "Time" in the original data into 
#   a POSIXlt vector, used in the x axis of some of the plots
get_power_data <- function () {
    # Download and unzip data
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                  "./household_power_consumption.zip", 
                  method = "curl",
                  quiet = TRUE)
    unzip("./household_power_consumption.zip")
    
    # Read all data
    raw_data <- read.table("./household_power_consumption.txt",
                           sep = ";",
                           header = TRUE,
                           stringsAsFactors = FALSE,
                           na.strings = "?")
    
    # Subset for the dates of interest
    isDateOfInterest <- grepl("^1/2/2007", raw_data$Date) | 
        grepl("^2/2/2007", raw_data$Date)
    
    filtered_data <- raw_data[isDateOfInterest, ]
    
    # Clean up the data a bit
    #   a) eliminate:
    #      "Global_intensity" column - not used in our plots
    #      "Date" and "Time" columns - later replaced by "DateTime"
    #   b) eliminate any row with NA on any remaining column
    
    DateTimeChar <- paste(filtered_data$Date, filtered_data$Time)
    
    filtered_data <- filtered_data[, 
                                   !(names(filtered_data) %in% 
                                         c("Date", "Time", "Global_intensity"))]
    
    filtered_data <- filtered_data[complete.cases(filtered_data), ]
    
    # Calculate the extra colum - DateTime and add via cbind
    DateTime <- strptime(DateTimeChar, "%d/%m/%Y %H:%M:%S")
    filtered_data <- cbind(DateTime, filtered_data)
    
    return(filtered_data)
}

# Main Script

# Read data
power_data <- get_power_data()

# Tweak some graphic parameter to make it similar to reference plots
par(cex=0.9, cex.axis=0.9, cex.lab=0.9, cex.main=0.9, cex.sub=0.9)
par(mfrow=c(1,1))

# Make the plot
#   Initial plot with Sub_metering_1, the add the others as lines
plot(power_data$DateTime, 
     power_data$Sub_metering_1, 
     type="l", 
     xlab="", 
     ylab="Energy sub metering")
lines(power_data$DateTime, 
      power_data$Sub_metering_2, 
      col="red")
lines(power_data$DateTime, 
      power_data$Sub_metering_3, 
      col="blue")

# Add legend
legend("topright", 
       inset=c(-0.015, -0.005),
       bg = "transparent",
       col=c("black", "red", "blue"), 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       lty="solid",
       cex=0.7)

# Copy to PNG file
dev.copy(png, file="plot3.png", width = 480, height= 480)
dev.off()