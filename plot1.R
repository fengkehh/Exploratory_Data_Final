# Plot 1

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

# Compute number of tons of PM2.5 emissions from all sources PER YEAR by summing
# the emission from each source from the same year together.

year_sum <- aggregate(data$Emissions, by = list(data$year), FUN = sum)

names(year_sum) <- c('year', 'totalemission')

# Plot to a png file
png('plot1.png')
plot(year_sum$year, year_sum$totalemission, pch = 16, xlab = 'Year', 
     ylab = 'Total Annual Emission (tons)', 
     main = 'PM 2.5 Emissions from All Sources vs Year in the U.S.')

abline(lm(totalemission ~ year, data = year_sum))
dev.off()

# Variable clean up
rm(list = setdiff(ls(), c('data', 'code')))