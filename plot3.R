# Plot 3
library('ggplot2')

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


# Compute yearly emissions grouped by type for baltimore
baltimore <- data[data$fips == '24510' , ]

baltimore_type_sum <- aggregate(baltimore$Emissions, 
                                by = list(baltimore$year, baltimore$type), 
                                FUN = sum)

names(baltimore_type_sum) <- c('year', 'type', 'totalemission')

baltimore_type_sum$type <- factor(baltimore_type_sum$type)

# Plot and save to a png file
qplot(year, y = totalemission, data = baltimore_type_sum, 
           color = type, 
           geom = 'line',
           xlab = 'Year',
           ylab = 'Total Annual Emissions (tons)', 
           main = 'Total Annual Emissions by Type in Baltimore City vs Year') +
    theme(plot.title = element_text(hjust = 0.5))

ggsave(file = 'plot3.png', width = 10)

# Variable clean up
rm(list = setdiff(ls(), c('data', 'code')))
