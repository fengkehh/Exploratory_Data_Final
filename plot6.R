# Plot 6

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

# Compute total emission from MV in Baltimore City & Los Angeles County

mv <- data[data$SCC %in% target$SCC & 
               (data$fips == '24510' | data$fips == '06037'),]
mv_annual <- aggregate(mv$Emissions, by = list(mv$year, mv$fips), FUN = sum)

names(mv_annual) <- names(mv_annual) <- c('year', 'fips', 'totalemission')

# Cmopute CHANGE in total emission per year (year N+1 - year N)

delta <- function(fips) {
    # Extract portion of the data frame that matches fips
    matched <- mv_annual[mv_annual$fips == fips,]
    
    # Compute emission delta
    result <- data.frame(deltaemission = -1*diff(matched$totalemission),
                        year = matched$year[2:nrow(matched)],
                        fips = matched$fips[2:nrow(matched)])
    return(result)
}


delta_annual <- rbind(delta('24510'), delta('06037'))
 
# Plot and save to a png file
library(ggplot2)

delta_annual$fips <- factor(delta_annual$fips, 
                            labels = c('Baltimore City', 'LA County'))

qplot(x = year, y = deltaemission, data = delta_annual,
      color = fips,
      geom = 'line',
      xlab = 'Year',
      ylab = 'Delta Total Annual Emission (tons)') + 
      labs(title = 'Change of Total Annual Emissions between Baltimore City and
      Los Angeles County by Year End',
            subtitle = 'Positive indicates DECREASE') +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5))
    

ggsave('plot6.png')

# Compute total delta over the years
total <- aggregate(delta_annual$deltaemission, by = list(delta_annual$fips), 
                   FUN = sum)
cat(sprintf('Total for Baltimore City: %f tons\nTotal for LA County: %f tons', 
        total$x[1], total$x[2]))

# Variable clean up
rm(list = setdiff(ls(), c('data', 'code')))

