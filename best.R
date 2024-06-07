##R Programming Assignment # 3: Best Function
##Author: Isaac Cormier
##Date: 2024/05/24


##Find the best hospital in a state

##The best() function takes the name of a state and a specified outcome ("heart attack", "heart failure", or "pneumonia") and returns the name of the
##hospital with the lowest 30-day mortality rate for the specific outcome and specified state.

library(dplyr)

best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE) %>% 
                select(Hospital = Hospital.Name,
                       StateName = State,
                       HeartAttack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                       HeartFailure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                       Pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        
        ## Check that state and outcome are valid
        state_list <- c(unique(df$StateName))
        state <- toupper(state)
        if(sum(grepl(paste('^', state,'$', sep = ""), state_list, ignore.case = FALSE)) < 1) stop("invalid state")
        
        outcome_list <- c('heart attack', 'heart failure', 'pneumonia')
        outcome <- tolower(outcome)
        if(sum(grepl(paste('^', outcome,'$', sep = ""), outcome_list, ignore.case = FALSE)) < 1) stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if (outcome == 'heart attack'){
                df$HeartAttack <- as.numeric(df$HeartAttack)
                df_flt <- filter(df, df$StateName == state & df$HeartAttack != "Not Available") %>%
                        arrange(HeartAttack, Hospital)
               return(df_flt$Hospital[1])}
        
        if (outcome == 'heart failure'){
                df$HeartFailure <- as.numeric(df$HeartFailure)
                df_flt <- filter(df, StateName == state) %>%
                        arrange(HeartFailure, Hospital)
                return(df_flt$Hospital[1])}
                
        if (outcome == 'pneumonia'){
                df$Pneumonia <- as.numeric(df$Pneumonia)
                df_flt <- filter(df,StateName == state) %>% 
                        arrange(Pneumonia, Hospital)
                return(df_flt$Hospital[1])}
                
}

