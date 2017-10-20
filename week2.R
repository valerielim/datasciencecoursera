###### Week 2 
###### Assignment + Quiz

# ---------------------------------------------------------------------------- #
# Assignment
# loops

if(x>3) { 
    y <- 10
} else {
    y < 3
}

# 1. count number of files in dir.
setwd("/Users/valerielim/Documents/Rfiles/JH Coursera/specdata")
files <- list.files(path="/Users/valerielim/Documents/Rfiles/JH Coursera/specdata", 
                    pattern="*.csv", full.names=T, recursive=FALSE)
length(files)

setwd("/Users/valeriehy.lim/Documents/Learning/specdata/")
files <- list.files(path="/Users/valeriehy.lim/Documents/Learning/specdata/", 
                    pattern="*.csv", full.names=T, recursive=FALSE)


# 2. Make loop for x number of files:
x <- 0
for (i in 1:length(files)){
    print(paste0("reading file ", i))
}

# OR 

count <- 0
while(count < length(files)){
    print(count)
    count <- count+1
}

# 3. Bind all files together within loop:
main_file <- read.csv("001.csv")
for (i in 2:length(files)){
    print(paste0("Reading file ", i, "..."))
    filename <- sprintf("%03d.csv", i)
    new_file <- read.csv(filename) 
    main_file <- rbind(main_file, new_file)
    print("Done!")
}
print("Done with all!")

# 4. TEST
range_ID <- c(1:10)
monitors <- dplyr::filter(main_file, ID %in% range_ID)
mean(monitors$nitrate, na.rm=TRUE)

# ASSEMBLE AS FUNCTION

pollutantmean <- function(directory, pollutant, range_ID){
    library(dplyr)
    
    # set directory
    setwd(paste0("./", directory, "/"))
    print(paste0("Directory set to:", getwd()))
    
    # merge files
    main_file <- read.csv("001.csv")
    for (i in 2:length(files)){
        print(paste0("Reading file ", i, "..."))
        filename <- sprintf("%03d.csv", i)
        new_file <- read.csv(filename) 
        main_file <- rbind(main_file, new_file); print("Done!")
    }
    
    # select desired monitors
    print(paste0("Selecting monitors in:", id))
    monitors <- filter(main_file, ID %in% range_ID)
    
    # get average
    print("Calculating average...")
    result <- mean(monitors$pollutant, na.rm=TRUE)
    return(result)
}

### FASTER VERSION

pollutantmean_fast <- function(pollutant, id=1:332){
    library(dplyr)
    # select desired monitors
    print(paste0("Selecting monitors in: ", as.character(id)))
    monitors <- dplyr::filter(main_file, ID %in% id)
    
    # get average
    print("Calculating average...")
    result <- mean(monitors[[pollutant]], na.rm=TRUE)
    return(result)
}

# Test cases
pollutantmean_fast("sulfate", 1:10)  # 4.064
pollutantmean_fast("nitrate", 70:72) # 1.706
pollutantmean_fast("sulfate", 34) #1.477
pollutantmean_fast("nitrate") # 1.7029

# ---------------------------------------------------------------------------- #

# Check num dates per monitor recorded
complete_slow <- function(directory, id=1:332){
    library(dplyr)
    
    # set directory
    setwd(paste0("./", directory, "/"))
    print(paste0("Directory set to:", getwd()))
    
    # merge files
    main_file <- read.csv("001.csv")
    for (i in 2:length(files)){
        print(paste0("Reading file ", i, "..."))
        filename <- sprintf("%03d.csv", i)
        new_file <- read.csv(filename) 
        main_file <- rbind(main_file, new_file)
        print("Done!")
    }
    
    # count all monitors
    counters <- main_file %>%
        dplyr::filter(!is.na(nitrate)&!is.na(sulfate)) %>%
        group_by(ID) %>%
        summarize(nobs = n())
    
    # select desired monitors
    desired_ids <- dplyr::filter(counters, ID %in% id)
    return(desired_ids)
}

# FASTER FUNCTION
complete_fast <- function(id=1:332){
    library(dplyr)
    counters <- main_file %>%
        dplyr::filter(!is.na(nitrate)&!is.na(sulfate)) %>%
        group_by(ID) %>%
        summarize(nobs = n())
    desired_ids <- dplyr::filter(counters, ID %in% id)
    return(desired_ids)
}

# test cases
complete_fast(id=c(1))
complete_fast(id=c(2,4,8,10,12))
complete_fast(id=c(30:25))
complete_fast(id=c(3))

# quiz cases
q5 <- complete_fast(id=c(6,10,20,34,100,200,310))
q5$nobs
# 228 148 ... 232


q6 <- complete_fast(id=54)
q6$nobs
# 219

# Question 7 (?)
set.seed(42)
q7 <- complete_fast(id=c(332:1))
use <- sample(332, 10)
print(q7[use, "nobs"])
use_results <- complete_fast(id=use)
    
# ---------------------------------------------------------------------------- #

corr_slow <- function(threshold){
    library(dplyr)
    
    # set directory
    setwd(paste0("./", directory, "/"))
    print(paste0("Directory set to:", getwd()))
    
    # merge files
    main_file <- read.csv("001.csv")
    for (i in 2:length(files)){
        print(paste0("Reading file ", i, "..."))
        filename <- sprintf("%03d.csv", i)
        new_file <- read.csv(filename) 
        main_file <- rbind(main_file, new_file)
        print("Done!")
    }
    
    # Find num complete obs per ID
    counters <- main_file %>%
        dplyr::filter(!is.na(nitrate)&!is.na(sulfate)) %>%
        group_by(ID) %>%
        summarize(nobs = n())
    
    # Select suitable monitors w threshold
    threshold <- as.numeric(threshold)
    suitable_monitors <- dplyr::filter(counters, nobs>threshold)
    monitor_ids <- as.vector(suitable_monitors$ID)
    
    # Find correlations
    holder <- c(0, 1)
    for (i in seq_along(monitor_ids)){
        ID_data <- dplyr::filter(main_file, ID==monitor_ids[i])
        correlation <- cor(ID_data$nitrate, ID_data$sulfate, 
                           use="pairwise.complete.obs")
        holder <- append(holder, correlation)
    }
    return(holder[-(1:2)])
}


corr_fast <- function(threshold){
    library(dplyr)
    
    # Select suitable monitors w threshold
    threshold <- as.numeric(threshold)
    suitable_monitors <- dplyr::filter(counters, nobs>threshold)
    monitor_ids <- as.vector(suitable_monitors$ID)
    
    # Find correlations
    holder <- c(0, 1)
    for (i in seq_along(monitor_ids)){
        ID_data <- dplyr::filter(main_file, ID==monitor_ids[i])
        correlation <- cor(ID_data$nitrate, ID_data$sulfate, 
                           use="pairwise.complete.obs")
        holder <- append(holder, correlation)
    }
    return(holder[-(1:2)])
}

# Question 8 
cr <- corr(0)
cr <- sort(cr)
set.seed(868)
out <- round(cr[sample(length(cr), 5)], 4)
# [1]  0.2688  0.1127 -0.0085  0.4586
# [5]  0.0447

# Question 9 
cr <- corr(129)
cr <- sort(cr)
n <- length(cr)
set.seed(197)
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
# [1] 243.0000   0.2540   0.0504
# [4]  -0.1462  -0.1680   0.5969

# Question 10
cr <- corr(2000)
n <- length(cr)
cr <- corr(1000)
cr <- sort(cr)
print(c(n, round(cr, 4)))
# [1]  0.0000 -0.0190  0.0419  0.1901
