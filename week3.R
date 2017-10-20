# ---------------------------------------------------------------------------- #

# Week 3 cache data
# Assignment

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# ---------------------------------------------------------------------------- #

### Week 3
### Quiz solutions

# Q1 
v <- dplyr::filter(iris, Species=="virginica")
result <- mean(v$Sepal.Length)
result <- round(result)

# Q2
apply(iris[, 1:4], 2, mean)

# Q3
### All of these options will work:
sapply(split(mtcars$mpg, mtcars$cyl), mean)
with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)

# Q4
### horsepower = hp:
horsepower <- tapply(mtcars$hp, mtcars$cyl, mean)
round(abs(horsepower[3] - horsepower[1]))
# Ans: 127

# Q5
# execution of 'ls' will suspend at the beginning of the function 
# and you will be sent to the browser
