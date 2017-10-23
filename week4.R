# Title: week4.R
# Course: (2) R Programming
# Week 4 assignment: hospital data
# Date: 20 Oct 2017

# ---------------------------------------------------------------------------- #

Title: 11. Hospital_Data.csv
The Hospital_Data.csv table contains thirteen (13) fields. This table provides 
general Hospital information in response to a Hospital Compare search.


1. Provider Number: varchar (6) Lists the hospitals by their provider 
   identification number.
2. Hospital Name: varchar (50) Lists the name of the hospital.
3. Address 1: varchar (50) Lists the first line of the street address of the hospital.
4. Address 2: varchar (50) Lists the second line of the street address of the hospital.
5. Address 3: varchar (50) Lists the third line of the street address of the hospital.
6. City: varchar (28) Lists the city in which the hospital is located.
7. State: varchar (2) Lists the 2 letter State code in which the hospital is located.
8. ZIP Code: char (5) Lists the 5 digit numeric ZIP for the hospital.
9. County Name: char (15) Lists the county in which the hospital is located.
10. Phone Number: char (10) Lists the 10-digit numeric telephone number, including 
    area code, for the Hospital.
11. Hospital Type: char (25) Lists the type of hospital. The values are:
        a. Acute Care Hospital
        b. Acute Care – VA Medical Center
        c. Critical Access Hospital
        d. Children’s Hospital
12. Hospital Owner: varchar (44) Lists the type of ownership the Hospital falls 
    under. The values are:
        a. Government – Federal
        b. Government – Hospital District or Authority
        c. Government – Local
        d. Government – State
        e. Proprietary
        f. Voluntary non-profit – Church
        g. Voluntary non-profit – Other
        h. Voluntary non-profit – Private
        i. Not Available
13. Emergency Services: char (3) Returns “Yes” or “No” to specify whether or 
    not the hospital provides emergency services.

# ---------------------------------------------------------------------------- #
# Summary 

Title: 19. Outcome of Care Measures.csv

The Outcome of Care Measures.csv table contains forty seven (47) fields. 
This table provides each hospital’s risk-adjusted 30-Day Death (mortality) and 
30-Day Readmission category and rate.


1. Provider Number: varchar (6) Lists the hospitals by their provider 
   identification number.
2. Hospital Name: varchar (50) Lists the name of the hospital.
3. Address 1: varchar (50) Lists the first line of the street address of the hospital.
4. Address 2: varchar (50) Lists the second line of the street address of the hospital.
5. Address 3: varchar (50) Lists the third line of the street address of the hospital.
6. City: varchar (28) Lists the city in which the hospital is located.
7. State: varchar (2) Lists the 2 letter State code in which the hospital is located.
8. ZIP Code: char (5) Lists the 5 digit numeric ZIP for the hospital.
9. County Name: char (15) Lists the county in which the hospital is located.
10. Phone Number: char (10) Lists the 10-digit numeric telephone number, including 
    area code, for the Hospital.
11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk 
    adjusted rate (percentage) for each hospital.
12. Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart 
    Attack: varchar (50) Lists the mortality and readmission category in which the 
    hospital falls. The values are:
        • Better than U.S. National Average
        • No Different than U.S. National Average
        • Worse than U.S. National Average
        • Number of Cases too Small
13. Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from 
    Heart Attack: Lists the lower bound (Interval Estimate) for each hospital’s 
    risk-adjusted rate.
14. Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from  
    Heart Attack: Lists the upper bound (Interval Estimate) for each hospital’s 
    risk-adjusted rate.
15. Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart 
    Attack: varchar (5) Lists the number of Medicare patients treated for Heart 
    Attack by the Hospital.
16. Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists 
    the footnote value when appropriate, as related to the Heart Attack Outcome 
    of Care at the hospital.
17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the risk 
    adjusted rate (percentage) for each hospital.
18. Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart 
    Failure: varchar (50) Lists the mortality and readmission category in which  
    the hospital falls. The values are:
        a. Better than U.S. National Average
        b. No Different than U.S. National Average
        c. Worse than U.S. National Average
        d. Number of Cases too Small*
19. Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart 
    Failure: Lists the lower bound (Interval Estimate) for each hospital’s 
    risk-adjusted rate.
20. Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart 
    Failure: Lists the upper bound (Interval Estimate) for each hospital’s 
    risk-adjusted rate.
21. Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart 
    Failure: varchar (5) Lists the number of Medicare patients treated for Heart 
    Failure by the Hospital.
22. Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists
    the footnote value when appropriate, as related to the Heart Failure Outcome
    of Care at the hospital.
23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk 
    adjusted rate (percentage) for each hospital.
24. Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from 
    Pneumonia: varchar (50) Lists the mortality and readmission category in 
    which the hospital falls. The values are:
        • Better than U.S. National Average
        • No Different than U.S. National Average
        • Worse than U.S. National Average
        • Number of Cases too Small*
25. Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from 
    Pneumonia: Lists the lower bound (Interval Estimate) for each hospital’s 
    risk-adjusted rate.


# ---------------------------------------------------------------------------- #
    
# Load files
setwd("/Users/valeriehy.lim/Documents/learning/week4")
outcome <- read.csv("outcome-of-care-measures.csv")
hdata <- read.csv("hospital-data.csv")
options(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------- #

# Q1
# histogram of heart attacks
heartattackrates <- as.numeric(outcome[, 11])
hist(heartattackrates)

# Q2 
# Changed "outcome" to 'illness' for clarity. Options: (col)
### heart attack, 11
### heart failure, 17
### pneumonia, 23
### If there are ties, return by alphabetical order

# Handling errors
### If there is a spelling error for state, return the message "invalid state"
### If there is a spelling error for illness, return the message "invalid outcome"

# Overall: return the hospital name in that state with the lowest 30-day death rate

# EXtract relevant cols

best <- function(state, illness) {
    ## Read outcome data
    setwd("/Users/valeriehy.lim/Documents/learning/week4")
    outcome <- read.csv("outcome-of-care-measures.csv")
    
    ## Clean
    subsetdata <- outcome[,c(1,2,3,7,11,17,23)]
    colnames(subsetdata) <- c("ID", "Name", "Address", "State", 
                              "HA", "HF", "PN")
    subsetdata[subsetdata=="Not Available"] <- NA
    subsetdata <- transform(subsetdata, 
                            HA = as.numeric(HA),
                            HF = as.numeric(HF),
                            PN = as.numeric(PN))
    
    ## Check that state and outcome are valid
    realstates <- unique(subsetdata$State)
    realillness <- c("heart attack", "heart failure", "pneumonia")
    
    if(!(state %in% realstates)){
        print("Invalid state.") 
        break
    } else {
        if (!(illness %in% realillness)){
            print("Invalid illness.") 
            break
        } else {
            # print("Valid illness, valid state. Continue.")
        }
    }
    
    ## Convert illness string to non-spaced var
    if(illness=="heart attack"){
        colname = "HA" 
    } else {
        if(illness=="heart failure"){
            colname = "HF"
        } else {
            colname = "PN"
        } 
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    library(sqldf)
    query <- paste0('SELECT Name, State, ', colname, ' FROM subsetdata WHERE ', 
                    colname, ' NOT LIKE \"NA\" AND STATE LIKE \"', state, 
                    '\" ORDER BY ', colname, ' ASC, Name ASC LIMIT 3')
    out <- sqldf(paste(query))
    print(paste(out[1,1], "with mortality rate of", out[1,3]))
}

# Test cases
best("TX", "heart failure")
[1] "FORT DUNCAN MEDICAL CENTER with mortality rate of 8.1"

best("TX", "heart attack")
[1] "CYPRESS FAIRBANKS MEDICAL CENTER with mortality rate of 12"

best("MD", "heart attack")
[1] "JOHNS HOPKINS HOSPITAL, THE with mortality rate of 12.4"

best("MD", "pneumonia")
[1] "GREATER BALTIMORE MEDICAL CENTER with mortality rate of 7.4"

best("BB", "pneumonia")
[1] "Invalid state."

best("TX", "stuff")
[1] "Invalid illness."

# ---------------------------------------------------------------------------- #

rankhospital <- function(state, illness, num="best") {
    ## Read outcome data
    setwd("/Users/valeriehy.lim/Documents/learning/week4")
    outcome <- read.csv("outcome-of-care-measures.csv")
    
    ## Clean
    subsetdata <- outcome[,c(1,2,3,7,11,17,23)]
    colnames(subsetdata) <- c("ID", "Name", "Address", "State", 
                              "HA", "HF", "PN")
    subsetdata[subsetdata=="Not Available"] <- NA
    subsetdata <- transform(subsetdata, 
                            HA = as.numeric(HA),
                            HF = as.numeric(HF),
                            PN = as.numeric(PN))
    
    ## Check that state and outcome are valid
    realstates <- unique(subsetdata$State)
    realillness <- c("heart attack", "heart failure", "pneumonia")
    if(!(state %in% realstates)){
        print("Invalid state.") 
        break
    } else {
        if (!(illness %in% realillness)){
            print("Invalid illness.") 
            break
        } else {
            # print("Valid illness, valid state. Continue.")
        }
    }
    
    ## Convert illness argument to non-spaced var
    if(illness=="heart attack"){
        colname = "HA" 
    } else {
        if(illness=="heart failure"){
            colname = "HF"
        } else {
            colname = "PN"
        } 
    }
    
    ## Arrange hospitals by death rate
    library(sqldf)
    query <- paste0('SELECT Name, State, ', colname, ' FROM subsetdata WHERE ', 
                    colname, ' NOT LIKE \"NA\" AND STATE LIKE \"', state, 
                    '\" ORDER BY ', colname, ' ASC, Name ASC')
    out <- sqldf(paste(query))
    names(out) <- c("Name", "State", "Mortality")
    
    library(dplyr)
    out <- out %>%
        arrange(State, Mortality) %>%
        group_by(State) %>%
        mutate(Position = row_number())
    
    # Return desired result
    if(ranking=="best"){
        print(out[1,]) 
    } else {
        if(ranking=="worst"){
            print(tail(out, 1))
        } else {
            if(ranking>nrow(out)){
                print("NA")
                break
            } else {
                print(out[`ranking`,])
            }
        }
    }
    # end
}

# ---------------------------------------------------------------------------- #

rankall <- function(illness, num="best") {
    ## Read outcome data
    setwd("/Users/valeriehy.lim/Documents/learning/week4")
    outcome <- read.csv("outcome-of-care-measures.csv")
    
    ## Clean
    subsetdata <- outcome[,c(1,2,3,7,11,17,23)]
    colnames(subsetdata) <- c("ID", "Name", "Address", "State", 
                              "HA", "HF", "PN")
    subsetdata[subsetdata=="Not Available"] <- NA
    subsetdata <- transform(subsetdata, 
                            HA = as.numeric(HA),
                            HF = as.numeric(HF),
                            PN = as.numeric(PN))
    
    ## Check that state and outcome are valid
    realstates <- unique(subsetdata$State)
    realillness <- c("heart attack", "heart failure", "pneumonia")
    if(!(state %in% realstates)){
        print("Invalid state.") 
        break
    } else {
        if (!(illness %in% realillness)){
            print("Invalid illness.") 
            break
        } else {
            # Valid illness, valid state
            # Continue
        }
    }
    
    ## Convert illness string to nice string
    if(illness=="heart attack"){
        colname = "HA" 
    } else {
        if(illness=="heart failure"){
            colname = "HF"
        } else {
            colname = "PN"
        } 
    }
    
    ## Arrange hospitals by death rate
    library(sqldf)
    query <- paste0('SELECT Name, State, ', colname, ' FROM subsetdata WHERE ', 
                    colname, ' NOT LIKE \"NA\" AND STATE LIKE \"', state, 
                    '\" ORDER BY ', colname, ' ASC, Name ASC')
    out <- sqldf(paste(query))
    names(out) <- c("Name", "State", "Mortality")
    
    library(dplyr)
    out <- out %>%
        arrange(State, Mortality) %>%
        group_by(State) %>%
        mutate(Position = row_number())
    
    # Return desired result
    if(num=="best"){
        print(out[out$Position=="1",]) 
    } else {
        if(num=="worst"){
            print("You haven't figured this out.")
        } else {
            if(num>nrow(out)){
                print("NA")
                break
            } else {
                print(out[out$Position==`num`,])
            }
        }
    }
    # end
}

rankall("heart attack", "best")

out <- subsetdata %>%
    arrange(State, HA) %>%
    group_by(State) %>%
    mutate(Position = row_number())

out[out$Position=="1",]

# ---------------------------------------------------------------------------- #

# Test cases
rankhospital("TX", "heart failure", 4)
Name State Mortality Position
    1 DETAR HOSPITAL NAVARRO    TX       8.7        4



rankhospital("MD", "heart attack", "worst")
Name State Mortality Position
    1 HARFORD MEMORIAL HOSPITAL    MD      18.1       41

rankhospital("MN", "heart attack", 5000)
    1 NA    

# ---------------------------------------------------------------------------- #





### Quiz cases

best("SC", "heart attack")
[1] "MUSC MEDICAL CENTER with mortality rate of 12.9"

best("NY", "pneumonia")
[1] "MAIMONIDES MEDICAL CENTER with mortality rate of 7.4"

best("AK", "pneumonia")
[1] "YUKON KUSKOKWIM DELTA REG HOSPITAL with mortality rate of 9.7"

rankhospital("NC", "heart attack", "worst")
1 WAYNE MEMORIAL HOSPITAL    NC        19       83

rankhospital("WA", "heart attack", 7)
1 YAKIMA VALLEY MEMORIAL HOSPITAL    WA      13.9        7

rankhospital("TX", "pneumonia", 10)
1 SETON SMITHVILLE REGIONAL HOSPITAL    TX       9.1       10

rankhospital("NY", "heart attack", 7)
1 BELLEVUE HOSPITAL CENTER    NY      12.6        7



### Questions
# Referencing variables
library(sqldf)
query <- paste0('SELECT Name, State, ', colname, ' FROM subsetdata WHERE ', 
                colname, ' NOT LIKE \"NA\" AND STATE LIKE \"', state, 
                '\" ORDER BY ', colname, ' ASC, Name ASC')
out <- sqldf(paste(query))

but others like 
sprintf
as.symbol
as.name
[[]] https://stackoverflow.com/questions/26003574/r-dplyr-mutate-use-dynamic-variable-names
` `
dont work?
