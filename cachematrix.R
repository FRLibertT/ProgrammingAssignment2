## makeCacheMatrix()
## makeCacheMatrix() is a function which creates a function and stores objects in this function environnement

## Syntax : createdfunction <- makeCacheMatrix(x)
## The created function "createdfunction" is in fact a list of 4 functions (createdfunction$set, createdfunction$get, createdfunction$setinv, createdfunction$getinv).
## The input of created function "createdfunction" is a matrix which is expected to be invertible.
## Thus the environnement attached to the created function "createdfunction" contains the 4 functions, 
## the matrix x and the matrix invx (computed later with the function cacheSolve).

## set(x) : allows to store (replace) a new matrix x in the environnement of "createdfunction" without running makeCacheMatrix() once again
## get() : prints the stored matrix x
## setinv(invx) : stores the matrix invx in the environnement of "createdfunction"
## getinv() : prints the stored matrix invx
## NB : I add the logical variable computedinvx to the environnement of createdfunciton, which can be called with the 5th function createdfunction$getcomputedinvx
## the value of computedinvx x is FALSE if no inverted matrix is stored in invx, TRUE otherwise.


makeCacheMatrix <- function(x = matrix()) {
        computedinvx <- FALSE          		## setting default value of computedinvx to FALSE, since there no inverted matrix has been computed
        set <- function(y) {
                x <<- y
                computedinvx <<- FALSE		## setting default value of computedinvx to FALSE, when resetting the value of stored matrix x
        }
        get <- function() x
        setinv <- function(inverse) {
			invx <<- inverse
			computedinvx <<- TRUE
	  }
	  getcomputedinvx <- function() computedinvx
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv,
		 getcomputedinvx =  getcomputedinvx)
}

## cacheSolve()

## cacheSolve() is a function which takes for input a function (and its environnement) created by the makeCacheMatrix() function
## cacheSolve() checks if an inverted matrix exists in the environnement of the function in input 
	## If yes, it calls the existing value, 
	## If not, it computes the inverted matrix, returns it, then stores it in the function environnement

## Syntax : y<-cacheSolve(createdfunction)
## cacheSolve uses the 4 functions, the matrix and the inverted matrix (plus the logical value computedinvx) stored in the createdfunction environnement 
## cacheSolve tests the variable computedinvx
## If computedinvx is TRUE, then there is a computed inverted matrix invx stored in the environnement of createdfunction : 
	## the cacheSolve function returns the value of invx in the current environnement and stops there
## Else if computedinvx is FALSE, then there is no computed inverted matrix invx stored in the environnemnet of createdfunction. Invx must be computed.
	## The stored invertible matrix is called with the createdfunction$get() function
	## The inverted matrix is computed with the solve function, returned to the current environnement 
	## and stored in the createdfunction environnement using the createdfunction$setinv() function

cacheSolve <- function(x, ...) {

	  ## Tests if invx has already been computed, ie if computedinvx - which is called with the x$getcomputedinvx() -  is TRUE					
	  if (x$getcomputedinvx()) {				
		    message("getting cached data")			
	  	    return(x$getinv())
	  }

	  ## If not, computes invx	
        data <- x$get()
        invx <- solve(data, ...)
        x$setinv(invx)
	  invx
}
