########### Week 1
########### No assigment; just quiz (here)

# 1. The R language is a dialect of which of the following programming languages?
ans <- S 

# 2. Four freedoms:
# >>> restrict access to the source code
# >>> prevent users from using the software for undesirable purposes
# >>> freedom to sell software for any price

# 3. Atomic data types are:
# atomic refers to vectors; R has six vector types:
# logical, integer, real, complex, string/ character, raw. 
# Lists have elements, they do not have to be the same type, hence not atomic.

# 4. x <- 4L is an integer

# 5. x <- c(4, "a", TRUE) is a character

# 6. rbind(x, y)
x <- c(1,3,5)
y <- c(2,4,6)
z <- rbind(x,y); class(z)
# a matrix of two rows, three columns

# 7. Vectors in R must:
# ALL be of the same class.

# 8. What does x[[2]] give me?
# 
# >>>> Coercion rules in R for class(list)
# Order: logical -> integer -> numeric -> complex -> character

# 9. what happens when x+y?
x <- 1:4
y <- 2:3
z<- x+y
class(z) # integer vector, 3,5,5,7

# 10. Which R code ... 
x[x>10] <- 4

# 11. What are the colnames of the dataset? 
hw <- read.csv(file.choose())
str(hw)

# 12. Extract the first two rows... 
hw[1:2,] # 41, 36...

# 13. How many rows are there?
nrow(hw) # 153

#14. Extract the last two rows... 
hw[152:153,] # or
tail(hw) # 18, 20...

# 15. What is the value of the ozone in the 47th row?
hw[47,]

# 16. How many missing values are there in the Ozone column?
sum(is.na(hw$Ozone)) # 37

# 17. What is the mean of the ozone column?
mean(hw$Ozone, na.rm=TRUE) # 42.129

# 18. Extract subset of rows where ozone > 31, and temp > 90. 
# What is the mean of the solar.R in this subset?
q18 <- hw[(hw$Ozone > 31)&(hw$Temp>90),]
mean(q18$Solar.R, na.rm=TRUE) # 212.8

# 19. What is the mean of 'temp' when month = 6?
q19 <- hw[(hw$Month==6),]
mean(q19$Temp) # 79.1

# 20. What is the maximum ozone value in the month of may?
q20 <- hw[(hw$Month==5),]
max(q20$Ozone, na.rm=TRUE)

# ---------------------------------------------------------------------------- #
