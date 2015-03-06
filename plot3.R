mk_plot3<-function(verb=F){
    time <- rep(Sys.time(), 2)
    
    init <- read.table("household_power_consumption.txt", header=T, sep=";", 
                       nrow = 5, na.strings = "?", stringsAsFactors = F)
    classes <- sapply(init, class)
    rm(init)
    time[2] <- Sys.time()
    if (verb) {print(paste("Data classes determined.", difftime(time[2],time[1]),"seconds"))}
    
    data <- read.table("household_power_consumption.txt", header=T, sep=";",
                       colClasses = classes, , na.strings = "?", stringsAsFactors = F)
    time[2] <- Sys.time()
    if (verb) {print(paste("Data loaded in memory.", difftime(time[2],time[1]),"seconds"))}
    
    temp <- as.Date(data[[1]], format="%d/%m/%Y")
    data[[1]] <- temp
    rm(temp)
    time[2] <- Sys.time()
    if (verb) {print(paste("Strings converted to dates.", difftime(time[2],time[1]),"seconds"))}
    
    in_date <- as.Date("01/02/2007", format="%d/%m/%Y")
    in_mask <- data[[1]]>=in_date
    out_date <- as.Date("02/02/2007", format="%d/%m/%Y")
    out_mask <- data[[1]]<=out_date
    date_mask <- in_mask & out_mask
    
    sel_data <- data[date_mask,]
    time[2] <- Sys.time()
    if (verb) {print(paste("Date selection performed.", difftime(time[2],time[1]),"seconds"))}
    
    sel_data <- mutate(sel_data, TimeStamp=format(as.POSIXct(
        paste(sel_data$Date, sel_data$Time)), "%d/%m/%Y %H:%M:%S"))
    time[2] <- Sys.time()
    if (verb) {print(paste("TimeStamp created.", difftime(time[2],time[1]),"seconds"))}
    
    png(file="plot3.png")
    plot(strptime(sel_data$TimeStamp, format="%d/%m/%Y %H:%M:%S"), sel_data$Sub_metering_1, 
         ylab="Global Active Power (kWh)", xlab="", type="l",col="black")
    lines(strptime(sel_data$TimeStamp, format="%d/%m/%Y %H:%M:%S"), sel_data$Sub_metering_2, 
         type="l",col="red")
    lines(strptime(sel_data$TimeStamp, format="%d/%m/%Y %H:%M:%S"), sel_data$Sub_metering_3, 
         type="l",col="blue")
    legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col=c("black", "red", "blue"), lty=1)
    dev.off()
    time[2] <- Sys.time()
    if (verb) {print(paste("Graphic file exported.", difftime(time[2],time[1]),"seconds"))}
    
    sel_data
}

mk_plot3()