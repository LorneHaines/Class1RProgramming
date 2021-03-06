####################################################################
# Filename: rankhospital.R
# Date: 10/03/2016
# Purpose:Function reads in hospital data about patient outcome after
# treatment for heart attack heart, failure, and pneumonia. It then sorts
# the data and returns the name of the hospital with a specific rank
# Inputs: state of hospitals examining, condition, & requested rank
# OUtputs: Name of hopital with requested state condition and rank for 
# 30 day mortality rate 
####################################################################

# sets working directory
setwd(paste("C:/Users/Bob/Documents/Coursera/Course 2 R Programming",
            "/rprog%2Fdata%2FProgAssignment3-data", sep = ""))

rankhospital <- function(state, outcome, num = "best"){

    ## reads outcome data
    # reads in data about patient outcome after treatment
    outcome_data<- read.csv("outcome-of-care-measures.csv", 
                            colClasses= "character")
    
    #creates vector with indicies for mortality rates for specific conditions
    condit_mortal_indx <- c('heart attack' = 11, 'heart failure' = 17,
                            'pneumonia' = 23)
    
    # creates a vector with all state abbreviations including DC and territories
    state_terr <- sort(unique(outcome_data$State))
    
    # converts state input to upercase
    state_upper <- toupper(state)
    
    # converts condition to lowercase
    condition_lower <- tolower(outcome)
    
    ##check state valid
    if (state_upper %in% state_terr){
    } else {
        stop("invalid state")
    }
    
    ## check outcome valid
    if (condition_lower %in% c ("heart attack", "heart failure", "pneumonia")){
    } else {
        stop("invalid outcome")        
    }
    
    ## return hospital name with matching state and rank
    
    # makes data frame with information for requested state
    state_data <- subset(outcome_data, outcome_data$State == state_upper)
    
    # replaces string "not available" with an NA value for requested condition
    state_data_list <- replace(state_data[condit_mortal_indx[condition_lower]], 
            state_data[condit_mortal_indx[condition_lower]] == "Not Available", NA)
    
    # converts requested condition to a vector
    state_data_vect <- as.numeric(unlist(state_data_list))
        
    
    # sorts data frame with mortalitiy rates for specific state by condition 
    # na.last is defaulted to true. Variable Hospital.Name is used to break ties
    sort_condition_state_data <- state_data[order(state_data_vect,
                                state_data$Hospital.Name, na.last = NA),]
    
    # calculates number of hospitals with valid obs in state
    num_hospitals <- nrow(sort_condition_state_data)
    
    # checks to see if num is a character data type
    # if it is lowercases it and checks valid inputs
    if (is.character(num)){
        string_rank <- tolower(num)
        
        # returns best hospital
        if (num == "best"){
            return(head(sort_condition_state_data$Hospital.Name, n = 1))    
        } else if (num == "worst"){
            #returns worst hospital
            return(tail(sort_condition_state_data$Hospital.Name, n = 1))
        } else {
            # tells that input is invalid
            stop("invalid rank")
        }
    # returns NA if given rank is to large
    } else if (num_hospitals < num) {
        return(NA)
    } else{
        # returns name of hospital with lowest mortality rate
        requested_hos <- sort_condition_state_data$Hospital.Name[num]
    }
    requested_hos
}