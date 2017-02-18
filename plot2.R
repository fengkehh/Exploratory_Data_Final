# Plot 2

# Load data if necessary
data_load <- function() {
    # Check that both data files exist
    if (!prod(c('Source_Classification_Code.rds', 'summarySCC_PM25.rds') %in% dir())) {
        download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip', 
                      destfile = 'dataset.zip')
        unzip('dataset.zip')
    }
    
    code <- readRDS('Source_Classification_Code.rds')
    data <- readRDS('summarySCC_PM25.rds')
    
    return(list(code, data))
}


if (!prod(c('code', 'data') %in% ls())) {
    result <- data_load()
    code <- result[[1]]
    data <- result[[2]]
}

# Compute yearly total emissions for Baltimore City (fips = 24510)
baltimore <- data[data$fips == '24510' , ]
baltimore_sum <- aggregate(baltimore$Emissions, by = list(baltimore$year), 
                           FUN = sum)

names(baltimore_sum) <- c('year', 'totalemission')

# Plot to a png file
png('plot2.png')
plot(baltimore_sum$year, baltimore_sum$totalemission, pch = 16, xlab = 'Year', 
     ylab = 'Total Annual Emission (tons)', 
     main = 'PM 2.5 Emissions from All Sources vs Year in Baltimore City')

abline(lm(totalemission ~ year, data = baltimore_sum))
dev.off()

# Variable clean up
rm(list = setdiff(ls(), c('data', 'code')))