####################################################################
# Filename: rankall.R
# Date: 10/03/2016
# Purpose:Function reads in hospital data about patient outcome after
# treatment for heart attack heart, failure, and pneumonia. It then sorts
# the data and returns the name of the hospital with the requested rank. 
# This is done for each state based off mortality rates
# Inputs: Condition examined and rank requested for each state
# OUtputs: Name of hopital with lowest 30 day mortality rate for state
# and condition
####################################################################
# sets working directory
setwd(paste("C:/Users/Bob/Documents/Coursera/Course 2 R Programming",
            "/rprog%2Fdata%2FProgAssignment3-data", sep = ""))

rankall <- function(outcome, num = "best"){
    
    ## reads outcome data
    # reads in data about patient outcome after treatment
    outcome_data<- read.csv("outcome-of-care-measures.csv", 
                            colClasses= "character")
    
    #creates vector with indicies for mortality rates for specific conditions
    condit_mortal_indx <- c('heart attack' = 11, 'heart failure' = 17,
                            'pneumonia' = 23)
    
    # converts condition to lowercase
    condition_lower <- tolower(outcome)
    
    ## check outcome valid
    if (condition_lower %in% c ("heart attack", "heart failure", "pneumonia")){
    } else {
        stop("invalid outcome")        
    }
    
    ## For each state, find the hospital of the given rank
    # creates a vector with all state abbreviations including DC and territories
    state_terr <- sort(unique(outcome_data$State))
    
    # makes an empty data frame with columns 
    hospitals_bystate <- data.frame(hospital = rep(NA, length(state_terr)) ,
                                       state = rep(NA, length(state_terr)))
    
    # sets rownames of data frame
    rownames(hospitals_bystate) <- state_terr
    for (i in seq_along(state_terr)){
        # flag used later (true is not requesting worst entry)
        flag <- TRUE
        
        # makes data frame with information for requested state
        state_data <- subset(outcome_data, outcome_data$State == state_terr[i])
    
        # replaces string "not available" with an NA value for requested condition
        state_data_list <- replace(state_data[condit_mortal_indx[condition_lower]],
        state_data[condit_mortal_indx[condition_lower]] == "Not Available", NA)
    
        # converts requested condition to a vector
        state_data_vect <- as.numeric(unlist(state_data_list))
        
    
        # sorts data frame with values for specific state by condition 
        # na.last = NA to remove all NA values.Variable Hospital.Name is used to break ties
        sort_condition_state_data <- state_data[order(state_data_vect, 
                                    state_data$Hospital.Name, na.last = NA),]
        
        # calculates number of hospitals with valid obs in state
        num_hospitals <- nrow(sort_condition_state_data)
        
        # checks to see if num is a character data type
        # if it is lowercases it and checks valid inputs
        if (is.character(num)){
            string_rank <- tolower(num)
            
            # sets num = 1 when input is best
            if (num == "best"){
                num <- 1    
            } else if (num == "worst"){
                #inserts value of worst observation into table
                hospitals_bystate$hospital[i] <- tail(sort_condition_state_data$Hospital.Name, n = 1)
                hospitals_bystate$state[i] <- state_terr[i]
                flag <- FALSE
            } else {
                # tells that input is invalid
                stop("invalid rank")
            }
        }
            # inserts value NA if given rank is to large for state
        if ((num_hospitals < num) & (flag == TRUE))  {
            hospitals_bystate$hospital[i] <- NA
            hospitals_bystate$state[i] <- state_terr[i]
        } else if (flag == TRUE){
            # the rank is valid and is used to update the data frame
    
            # uses the specified rank to add hospital name and state to data frame
            state_rank_name <- sort_condition_state_data$Hospital.Name[num]
            hospitals_bystate$hospital[i] <- state_rank_name
            hospitals_bystate$state[i] <- state_terr[i]
        }
    }
    # returns vector with hospitals of requested rank by state
    hospitals_bystate
}