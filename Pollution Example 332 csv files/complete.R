####################################################################
# Filename: complete.R
# Date: 9/21/2016
# Purpose:Reads in air polutant data from a set of 332 csv files
# returns data frame showing number of observations at each site
# Arguements: directory- where csv files located. Id vector of sites to look at
####################################################################

complete <- function(directory, id = 1:332) {
    # sets working directory
    #setwd("F:\\Coursera\\Course 2 R Programming\\specdata")
    setwd(directory)
    
    # generates character vector with all file names
    monitor_num <- list.files()
    # generates vector with file names requested
    req_file <- monitor_num[id]
    
    # removes na values and combines requested data into
    # data frame complete_obs
    complete_obs <- data.frame()
    for (i in seq_along(req_file)){
        monitor_data<- read.csv(req_file[i])
        clean_data <- na.omit(monitor_data)
        complete_obs <- rbind(complete_obs, clean_data)
    } 
    
    # creates a frequency table for the number valid obs per site
    complete_freq <- data.frame(table(complete_obs$ID))
    
    # adds titles to columns}
    colnames(complete_freq) <- c("id", "nobs")
    
    complete_freq
}