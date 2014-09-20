# makeCacheMatrix and cacheSolve work together to provide a means of computing the inverse of
# a given matrix and caching the results for future reference.
# The input matrix *is assumed* be invertible and no error checking is done to determine if this is the case.

# A call to makeCacheMatrix(matrix) should be done first to create the 'cacheMatrix' (cacheable matrix) object.
# This function returns a list object containing 4 functions to get / set the contents
# of the 'input' matrix, and the cached copy of it's inverse (the solution).

# A NULL input matrix may be supplied when calling makeCacheMatrix().
# In this case the input matrix may be specified later by calling the setinput() function of 
# the cacheMatrix object and providing an invertible matrix as the argument to this function.

# Once the cacheMatrix object is created it should be passed as an argument to
# cacheSolve() in order to compute the inverse of the matrix and to store this in cache.
# Subsequent calls to cacheSolve will return the cached result unless the input data
# is modified by calling the setinput() function of the cacheMatrix object.
# No error checking is done on the contents of the input matrix to protect against
# empty or otherwise unsolvable input data.

# The persistent state is achieved by use of the <<- assignment operator.

# See http://www.r-bloggers.com/environments-in-r/ for a good explanation of how this works in R

###################################################################################################

# Code:

makeCacheMatrix <- function(input.m = matrix()) {

# makeCacheMatrix provides a cacheable matrix object (manageable through 4 functions
# returned to the caller) which provides persistent storage of
        
# (a) the matrix data itself
# (b) the results of a (potentially) computationally intensive operation on the input data
# in this case the inverse of the matrix

# Function available in the makeCacheMatrix object are:

# makeCacheMatrix(matrix) creates a new 'cacheMatrix' object.
# It takes as input a normal R matrix object (which must be invertable).
# NULL may also be supplied as the argument to this function

# getinput() retrieves the last stored input matrix data
# setinput(matrix) updates the internally stored matrix data (not the solution data)
# getsolution() retrieves the last stored solution data
# setsolution(matrix) updates the internally stored solution data (not the input matrix data)


        # This matrix will hold the solution (inverse) of the input matrix
        # by default this is NULL for (as yet) unsolved matrices
        solution.m <- NULL
        
        
        # Define the function that lets us set the contents of input.m
        setinput <- function (inmatrix.m) {
                
                # Update 'input' with the matrix data from our caller
                # The assignment is done not to the execution environment of this function,
                # but to it's parent environment.
                input.m <<- inmatrix.m
                
                # Reset the cached result matrix to NULL to indicate that the input has changed
                # and the solution (inverse) is not yet cached.
                # The assignment is done not to the execution environment of this function,
                # but to it's parent environment
                solution.m <<- NULL
        }
        
        
        # Define the function that lets us retrieve the last stored matrix in input.m
        # input.m is ultimately retrieved not from the execution environment, but it's parent.
        getinput <- function () {
                input.m
        }
        
        
        # Define the function that lets us store a cached solution to the input matrix
        setsolution <- function(insolution.m) {
                # Save the solution matrix being provided from our caller
                # The assignment is done not to the execution environment of this function,
                # but to it's parent environment.
                solution.m <<- insolution.m
        }
        
        
        # Define the function that lets us retrieve the last cached solution matrix
        # solution.m is ultimately retrieved not from the execution environment, but it's parent
        getsolution <- function() {
                solution.m
        }
        
        
        # Return a list object containing the functions defined above. These may be used to get / set the
        # input matrix data as well as the solution data.
        list(setinput = setinput, getinput = getinput, setsolution = setsolution, getsolution = getsolution)
}




cacheSolve <- function(cmobj, ...) {
        
# cacheSolve(cmobj, ...) computes and returns the solution (inverse) of the input matrix stored as
# persistent environment info in the cacheMatrix object provided as it's FIRST argument (cmobj). 
# The solution is computed with the internal solve() R function.
# If possible cacheSolve retrieves the solution data from the internal cache (parent environment)
# of the cmobj object, otherwise it computes this on first invocation and then caches the result. 
# Optional arguments may be provided to cacheSolve and these will be passed unmodified to the 
# solve() function of R (if they are present).        
        
        # Start working...
        
        # Fetch the result currently stored in cache
        solution.m <- cmobj$getsolution()
        
        # If this is *not* NULL then simply return this to our caller
        if (!(is.null(solution.m))) {
                message("Retrieving cached solution ... ")
                return(solution.m)
        }
        
        # No valid cached solution is available
        message("No cached result available - computing solution ...")
        
        # Fetch the matrix data stored in the cmobj object provided to us
        input.m <- cmobj$getinput()
        
        # Compute the inverse of this matrix
        # Any additional parameters supplied from our caller are applied here
        solution.m <- solve(input.m, ...)
        
        # And call the setsolution function of the cmobj object to store the results in cache
        cmobj$setsolution(solution.m)
        
        # Finally - return the requested solution to our caller
        solution.m
}
