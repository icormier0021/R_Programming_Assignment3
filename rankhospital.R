##R Programming Assignment # 3: rankhospital Function
##Author: Isaac Cormier
##Date: 2024/05/28


##Rank the hospitals by outcome in a state

##The rankhospital() function takes the name of a state, a specified outcome ("heart attack", "heart failure", or "pneumonia"), and
##hte ranking of a hospital in that state for that specified outcome, and returns a character vector of the hospital that has the
##ranking specified by the num argument.

library(dplyr)

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE) %>% 
                select(Hospital = Hospital.Name,
                       StateName = State,
                       HeartAttack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                       HeartFailure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                       Pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        
        df$HeartAttack <- as.numeric(df$HeartAttack)
        df$HeartFailure <- as.numeric(df$HeartFailure)
        df$Pneumonia <- as.numeric(df$Pneumonia)
        
        ## Check that state and outcome are valid
        state_list <- c(unique(df$StateName))
        state <- toupper(state)
        if(sum(grepl(paste('^', state,'$', sep = ""), state_list, ignore.case = FALSE)) < 1) stop("invalid state")
        
        outcome_list <- c('heart attack', 'heart failure', 'pneumonia')
        outcome <- tolower(outcome)
        if(sum(grepl(paste('^', outcome,'$', sep = ""), outcome_list, ignore.case = FALSE)) < 1) stop("invalid outcome")

        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        list <- filter(df, df$StateName == state)
        hospital_list <- c(unique(list$Hospital))
        if(is.numeric(num) & num > length(hospital_list)){
                return(NA)}

        if (outcome == 'heart attack' & is.numeric(num)){
                df_flt <- filter(df, df$StateName == state & !is.na(df$HeartAttack)) %>% 
                        arrange(HeartAttack, Hospital)
                return(df_flt$Hospital[num])
                
        } else if (outcome == 'heart attack' & num == 'best'){
                df_flt <- filter(df, df$StateName == state & !is.na(df$HeartAttack)) %>% 
                        arrange(HeartAttack, Hospital)
                return(df_flt$Hospital[1])
        } else if (outcome == 'heart attack' & num == 'worst'){
                df_flt <- filter(df, df$StateName == state & !is.na(df$HeartAttack)) %>% 
                        arrange(HeartAttack, Hospital)
                return(df_flt$Hospital[length(df_flt$HeartAttack)])}
        
        if (outcome == 'heart failure' & is.numeric(num)){
                df_flt <- filter(df, df$StateName == state & !is.na(df$HeartFailure)) %>% 
                        arrange(HeartFailure, Hospital)
                return(df_flt$Hospital[num])
        } else if (outcome == 'heart failure' & num == 'best'){
                df_flt <- filter(df, df$StateName == state & !is.na(df$HeartFailure)) %>% 
                        arrange(HeartFailure, Hospital)
                return(df_flt$Hospital[1])
        } else if (outcome == 'heart failure' & num == 'worst'){
                df_flt <- filter(df, df$StateName == state & !is.na(df$HeartFailure)) %>% 
                        arrange(HeartFailure, Hospital)
                return(df_flt$Hospital[length(df_flt$Hospital)])}
        
        if (outcome == 'pneumonia' & is.numeric(num)){
                df_flt <- filter(df, df$StateName == state & !is.na(df$Pneumonia)) %>% 
                        arrange(Pneumonia, Hospital)
                return(df_flt$Hospital[num])
        } else if (outcome == 'pneumonia' & num == 'best'){
                df_flt <- filter(df, df$StateName == state & !is.na(df$Pneumonia)) %>% 
                        arrange(Pneumonia, Hospital)
                return(df_flt$Hospital[1])
        } else if (outcome == 'pneumonia' & num == 'worst'){
                df_flt <- filter(df, df$StateName == state & !is.na(df$Pneumonia)) %>% 
                        arrange(Pneumonia, Hospital)
                return(df_flt$Hospital[length(df_flt$Hospital)])}

}

