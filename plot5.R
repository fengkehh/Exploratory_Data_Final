# Plot 5

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

# Design: match the phrase 'Mobile' and 'On-Road' at least once with any other 
# stuff in front, in between or after from the code list to determine proper 
# source codes to extract. The definition of "motor vehicle" is from M-W:
# https://www.merriam-webster.com/dictionary/motor%2Bvehicle
expr <- '(Mobile)+.*(On-Road)+'

target <- code[grep(expr, code$EI.Sector),]

# Compute total emission from MV in Baltimore City

mv <- data[data$SCC %in% target$SCC & data$fips == '24510',]
mv_annual <- aggregate(mv$Emissions, by = list(mv$year), FUN = sum)

names(mv_annual) <- names(mv_annual) <- c('year', 'totalemission')

# Plot and save to a png file
png('plot5.png')
plot(mv_annual$year, mv_annual$totalemission, pch = 16, xlab = 'Year', 
     ylab = 'Total Annual Emission (tons)', 
     main = 'PM 2.5 Emissions from Motor Vehicle Sources in Baltimore City')

abline(lm(totalemission ~ year, data = mv_annual))
dev.off()

# Variable clean up
rm(list = setdiff(ls(), c('data', 'code')))
