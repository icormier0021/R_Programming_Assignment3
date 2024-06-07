##R Programming Assignment # 3: rankall Function
##Author: Isaac Cormier
##Date: 2024/05/28


##Ranking all the hospitals in a specific state

##The rankall() function takes a specified outcome (e.g., 'heart attack', 'heart failure', or 'pneumonia') and a hospital
##ranking and returns a 2-column data frame containing the hospital in each state that has the previously specified ranking

library(dplyr)

rankall <- function(outcome, num = "best") {
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
        
        outcome_list <- c('heart attack', 'heart failure', 'pneumonia')
        outcome <- tolower(outcome)
        if(sum(grepl(paste('^', outcome,'$', sep = ""), outcome_list, ignore.case = FALSE)) < 1) stop("invalid outcome")
        ## For each state, find the hospital of the given rank
        all <- data.frame(hospital=character(), state=character())
        for (i in state_list){
                if(is.numeric(num) & num > length(filter(df, df$StateName == i))){
                        all <- add_row(all,hospital=NA, state=i) %>% 
                                arrange(state)}
                
                if (outcome == 'heart attack' & is.numeric(num)){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$HeartAttack)) %>% 
                                arrange(HeartAttack, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[num], state=i) %>% 
                                arrange(state)
                } else if (outcome == 'heart attack' & num == 'best'){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$HeartAttack)) %>% 
                                arrange(HeartAttack, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[1], state=i) %>% 
                                arrange(state)
                } else if (outcome == 'heart attack' & num == 'worst'){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$HeartAttack)) %>% 
                                arrange(HeartAttack, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[length(df_flt$HeartAttack)], state=i) %>% 
                                arrange(state)}
                
                if (outcome == 'heart failure' & is.numeric(num)){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$HeartFailure)) %>% 
                                arrange(HeartFailure, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[num], state=i) %>% 
                                arrange(state)
                } else if (outcome == 'heart failure' & num == 'best'){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$HeartFailure)) %>% 
                                arrange(HeartFailure, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[1], state=i) %>% 
                                arrange(state)
                } else if (outcome == 'heart failure' & num == 'worst'){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$HeartFailure)) %>% 
                                arrange(HeartFailure, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[length(df_flt$HeartFailure)], state=i) %>% 
                                arrange(state)}
                
                if (outcome == 'pneumonia' & is.numeric(num)){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$Pneumonia)) %>% 
                                arrange(Pneumonia, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[num], state=i) %>% 
                                arrange(state)
                } else if (outcome == 'pneumonia' & num == 'best'){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$Pneumonia)) %>% 
                                arrange(Pneumonia, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[1], state=i) %>% 
                                arrange(state)
                } else if (outcome == 'pneumonia' & num == 'worst'){
                        df_flt <- filter(df, df$StateName == i & !is.na(df$Pneumonia)) %>% 
                                arrange(Pneumonia, Hospital)
                        all <-  add_row(all, hospital=df_flt$Hospital[length(df_flt$Pneumonia)], state=i) %>% 
                                arrange(state)}
                }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        return(all)
}
        