# Plot 4

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

# Design: match the phrases 'Comb' and 'Coal' at least once with any other stuff 
# in front, in between or after from the code list to determine proper source 
# codes to extract
expr <- '(Comb)+.*(Coal)+'

target <- code[grep(expr, code$Short.Name),]


# Compute total annual emission from just coal combustion related sources.
coal <- data[data$SCC %in% target$SCC,]
coal_annual <- aggregate(coal$Emissions, by = list(coal$year), FUN = sum)

names(coal_annual) <- c('year', 'totalemission')

# Plot and save to a png file
png('plot4.png')
plot(coal_annual$year, coal_annual$totalemission, pch = 16, xlab = 'Year', 
     ylab = 'Total Annual Emission (tons)', 
     main = 'PM 2.5 Emissions from Coal Sources vs Year in the U.S.')

abline(lm(totalemission ~ year, data = coal_annual))
dev.off()

# Variable clean up
rm(list = setdiff(ls(), c('data', 'code')))