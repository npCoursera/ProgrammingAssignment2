#########################################################
# Matrix I used for testing purposes
#########################################################
# m <- matrix(c(4:7), nrow = 2, ncol = 2, byrow = TRUE)

#########################################################
# makeCacheMatrix
#########################################################
makeCacheMatrix <- function(x = matrix())
	{
	  m<-NULL

	  set<-function(y)	### define set function
	    {
		x <<- y
		m <<- NULL
	    }

	### define get, setmatrix and getmatrix functions
	get<-function() x
	setmatrix<-function(solve) m <<- solve
	getmatrix<-function() m

	### return functions as a list ###
	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

#########################################################
# cacheSolve function
#########################################################
cacheSolve <- function(x=matrix())
{

    m<-x$getmatrix(x)   ### R is throwing me an error message here
				### "Error in x$getmatrix : $ operator is invalid for atomic vectors"
				### Turning in what I've done

    ### if the matrix is not NULL, retrive it and return it  ###
    if(!is.null(m))
	{
        message("getting cached data")	### let the user know cached matrix is being retrieved
        return(m)					### return m and exit function cacheSolve
	}

    ### if the matrix is NULL, then get the matrix, invert it, and cache it ###
    matrix<-x$get
    m<-solve(matrix)
    x$setmatrix(m)

    ### return the inverse of the matrix ###
    m
}
