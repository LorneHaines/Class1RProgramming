####################################################################
# Filename: pollutantmean.R
# Date: 9/21/2016
# Purpose: Reads in air polutant data from a set of 332 csv files
# returns the mean readings for requested pollutant
# Arguements: directory- where to read files, pollutant- either sulfate
# or nitrate, & id a vector containing monitors want to look at
####################################################################
pollutantmean <- function(directory, pollutant, id = 1:332){
    # sets working directory
    #setwd(""F:\\Coursera\\Course 2 R Programming\\specdata")
    # sets directory to user specified input
    setwd(directory)
    
    # generates character vector with all file names
    monitor_num <- list.files()
    # generates vector with filenames requested
    req_file <- monitor_num[id]
  
    # combines data of requested pollutant type into 1 vector 
    # & removes NA values
    pollut_vector <-c()
    if(pollutant == "sulfate"){
        for (i in seq_along(req_file)){
            sulfate_data<- read.csv(req_file[i], colClasses = c("Date"= "NULL", "sulfate"= "numeric", "nitrate" = "NULL", "ID" = "NULL"))
            clean_sulfate <- sulfate_data[!is.na(sulfate_data)]
            pollut_vector <- c(pollut_vector, clean_sulfate)
        } 
        
    } else if(pollutant == "nitrate") {
        for (i in seq_along(req_file)){
        nitrate_data<- read.csv(req_file[i], colClasses = c("Date"= "NULL", "sulfate"= "NULL", "nitrate" = "numeric", "ID" = "NULL"))
        clean_nitrate <- nitrate_data[!is.na(nitrate_data)]
        pollut_vector <- c(pollut_vector, clean_nitrate)
        }
    } else{
        print("You did not enter a correct pollutant")
    }
    
    # calculates and returns mean for requested pollutant
    pollut_vector
    mean(pollut_vector)
}

