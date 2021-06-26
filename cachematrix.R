#### This makeCacheMatrix function creates a special "matrix" object that can cache its inverse ####
# Function returns the inverse of the matrix, it can set up & get the input values (i.e. special matrix object)
# as well as the new value (i.e. inverse of the matrix) to be stored in the cache of another environment
# from its calling environment.

makeCacheMatrix <- function(x = matrix()) {
  # x is a user input value to be created as a matrix of numeric values, it can be a square numeric or complex matrix.
  # function returns the inverse of the matrix: x, by using the r shiny solve function.
  # It will store the inverse of the matrix in a cache in another environment & can be retrieved again from the cache.
  
  # Initialize the new variable called: i (to NULL), for the result of the inverse of the matrix after calling the solve function.
  i <- NULL
  
  # Define a new function called: set, to setup the variables to be stored in another environment (using symbol: <<)
  set <- function(y) {
    # Assign variable: y to x, to be cached into another environment that is different from the current environment.
    # x is a local variable to this set function only.
    # Dynamic scoping is used in the y variable.
    # The value of y is looked up in the environment from which the function was called (i.e. from calling environment).
    x <<- y
    
    # Assigns NULL value to i variable, its value is from the calling function - in this case is: MakeCacheMatrix function.
    i <<- NULL
  }
  
  # Define a new function called: get, and returns the value of x.
  # The x variable in this get function, is referred to as lexical scoping - its value is looked up in the environment
  # in which the function was defined in.
  get <- function() x
  
  # Define a new function called: set_inverse, it returns the inverse of the matrix 
  # by using solve function (an r shiny function).
  # The result is stored in variable: i & cached into another environment (using symbol: <<).
  # variable i in the set_inverse function, is referred as lexical scoping, its value is looked up in the function
  # that it was defined in.
  set_inverse <- function(solve) i <<- solve
  
  # Define a new function called: get_inverse, it returns the value of i.
  get_inverse <- function() i
  
  # Output the list of functions: set, get, set_inverse & get_inverse, as a result of this makeCacheMatrix function.
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


#### This cacheSolve function, computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# It first checks to see if the inverse of the matrix exists (and the matrix has not changed).
# If exists, a message is displayed onto the console and returns the existing inverse value from the cache.
# If it doesn't exist, then it will compute the inverse of the matrix, by using the R shiny "solve" function. ####
cacheSolve <- function(x, ...) {
  # Calling the get_inverse function (from the 'makeCacheMatrix' function), which returns the inverse of 'x'
  # x is from the result of calling the 'makeCacheMatrix' function, which is an input to this function.
    i <- x$get_inverse()
    # Checks to see if the variable: i, is not null.
    if(!is.null(i)) {
      #Issue out a message to the console, getting cached data.
      message("getting cached data")
      # Function returns the value of i. Exit out of the function as it does not need to re-calculate the inverse again.
      # The value of i, is the result of the inverse of the matrix,
      # which was already calculated as part of the 'makeCacheMatrix' function
      return(i)
    }
    # Getting the special "matrix" data, from the input function: x, which is from the 'makeCacheMatrix' function,
    # before the inverse computation was applied.
    data <- x$get()
    # Call the r shiny solve function to return the inverse of the matrix.
    i <- solve(data, ...)
    # Call the set_inverse function, to store the value of i into a cache.
    # the set_inverse function is from the 'makeCacheMatrix' function, which is the input 'x' variable in this
    # 'cacheSolve' function.
    x$set_inverse(i)
    
    ## Return a matrix that is the inverse of 'x', as a result of calling the solve function.
    i
}


#### Test the above functions ####
# Setup Matrix
# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

# imat <- makeCacheMatrix(m1)

# first call
# csolve <- cacheSolve(imat)

# second call to check if the message is returned with the existing value from the solve function.
# csolve <- cacheSolve(imat)

# test another matrix

# m2 <- matrix(c(1,2,3,4),2,2)

# imat2 <- makeCacheMatrix(m2)
# csolve2 <- cacheSolve(imat2)

#csolve2 <- cacheSolve(imat)




