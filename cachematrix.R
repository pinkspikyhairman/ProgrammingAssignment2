# The makeCacheMatrix() function creates a 'special' matrix object.
# The object comprises the matrix, its inverse, and a list of 4 functions that are used to get and set the matrix and its inverse:
# set(x), get(), setinv(inverse), getinv()
# note the use of "<<-" to reference parent function variables from within functions

makeCacheMatrix <- function(x = matrix()) {
# argument 'x' is the matrix and the cached inverse 'inv' is set to NULL
inv <- NULL
# the set function is used to change the value of the 'special' matrix once created
# the inverse 'inv' is set to null
set <- function(y) {
  x <<- y
  inv <<- NULL
}
# the get() function returns the matrix itself, ie. 'x'
get <- function() x
# the setinv() function caches the argument in variable 'inv'
setinv <- function(inverse) inv <<- inverse
# the getinv() function returns (the inverse cached in) variable 'inv'
getinv <- function() inv
# the following list is actually a list of the functions defined above
# so, the functions are invoked using named elements of the list
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)

# examples:
#
# > m22 <- matrix(c(0,1,1,1),nrow=2,ncol=2)     make an invertable 2x2 matrix, m22
# > cm22 <- makeCacheMatrix(m22)                make a cacheable version of m22
#
# > cacheSolve(cm22)                            use cacheSolve to calculate inverse
# calculating and caching inverse               first time its calculated and cached
# [,1] [,2]
# [1,]   -1    1
# [2,]    1    0
# >
# > cacheSolve(zzz)                             use cacheSolve to calculate inverse again
# cached inverse retrieved                      this time inverse is returned from cache
# [,1] [,2]
# [1,]   -1    1
# [2,]    1    0
# >
# > cm22$set(matrix(c(1,0,1,1),nrow=2,ncol=2))  change value of matrix
# > cacheSolve(cm22)                            this time its calculated and cached
# calculating and caching inverse
# [,1] [,2]
# [1,]    1   -1
# [2,]    0    1
# >

# The cacheSolve() function return the inverse of a 'special' matrix created by the makeCacheMatrix() function
# if the inverse of this matrix has been calulated before, the result returned it the cached value.
# if this is the first time, or the 'special' matrix has changed, the inverse is recalculated and then saved in
# the cache. Not a requiremet of the assignment, but this function will work with ordinary matrix objects as
# well as 'special' matrix objects.

cacheSolve <- function(x, ...) {
  
  # check argument is a 'special' matrix. 
  # if the argument is not a list object or the required functions are not in the list, then always calulate inverse
  if (typeof(x) == "list") {
    # note: could not combine conditions in a single 'if' because R gives error:
    #     "Error in as.environment(where) : invalid 'pos' argument"
    # when x is not a 'special' matrix because it evaluates all conditions, even after failing the '== "list"' condition
    if ( exists('set', where=x) & 
          exists('get', where=x) & 
          exists('setinv', where=x) & 
          exists('getinv', where=x) 
        ) {
        # use the getinv() function of 'cachematrix' object 'x' to retrieve inverse of matrix from cache if present
        inv <- x$getinv()
        # check if inverse was retrieved, ie. not NULL
        if(!is.null(inv)) {
          # check that it is actually a matrix
          if(!is.matrix(inv)) stop("getinv() did not return type 'matrix'")
          message("cached inverse retrieved")
          return(inv)
        }
        message("calculating and caching inverse")
        # get matrix from 'cachematrix' object 'x' using get() function and calculate inverse using R solve() function
        inv <- solve(x$get())
        # store inverse in 'cachematrix' object 'x' using setinv() function
        x$setinv(inv)
        # return inverse
        return(inv)
      } 
  }     
  # not a 'special' matrix, so just calculate and return inverse
  message("argument is not a 'special' matrix")
  inv <- solve(x)
  return(inv)
}
