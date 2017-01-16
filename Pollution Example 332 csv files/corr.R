####################################################################
# Filename: corr.R
# Date: 9/21/2016
# Purpose:Reads in air polutant data from a set of 332 csv files
# Finds correlations of all sites meeting threshold for min complete obs
# Arguements: directory- where csv files located. threshold- min # complete
# observations at a site
####################################################################

# sets up function and its arguements
corr <- function(directory, threshold = 0) {
    # sets working directory
    #setwd("F:\\Coursera\\Course 2 R Programming\\specdata")
    setwd(directory)
    
    # generates character vector with all file names
    monitor_num <- list.files()
    
    corr_coeff<- c()
    
    # reads in each site, removes NA values, counts
    # total valid observations
    for (i in seq_along(monitor_num)){
        monitor_data<- read.csv(monitor_num[i])
        clean_data <- na.omit(monitor_data)
        num_comp <- sum(complete.cases(monitor_data))
        
        # takes the correlation if the # of complete obs is valid
        # stores in vector corr_coeff for each site
        if (num_comp >= threshold) {
            current_corr <- cor(monitor_data$sulfate, monitor_data$nitrate, use = "na.or.complete")
            corr_coeff<-c(corr_coeff, current_corr)
        }
    }
    
    # removes possible NA values if site had a pollutant with 
    # no observations
    if (length(corr_coeff) != 0) {
        corr_coeff <- na.omit(corr_coeff)
    } else {
        corr_coeff <-c()
    }
    
    #returns vector of correlation coefficients
    corr_coeff
}