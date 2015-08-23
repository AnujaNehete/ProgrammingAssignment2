## The makeCacheMatrix function creates a special matrix with getter and setter methods for the matrix and its inverse
## The cacheSolve functions returns the cached inverse of the input matrix if present, else calculates the inverse of the matrix
## Note :It is assumed that the input matrix is invertible
## 
## The makeCacheMatrix function creates a special matrix ,
## It returns a list of 4 functions
## set -sets the input matrix
## get -returns the input matrix
## setinv -sets the inverse of the input matrix
## getinv -returns the inverse of the input matrix
makeCacheMatrix <- function(x = matrix(data=NA,nrow=0,ncol=0)) {
	inv_x <- matrix(data=NA,nrow=0,ncol=0)
	set <-function(y){
		x <<- y
		inv_x <<-matrix(data=NA,nrow=0,ncol=0)
	}
	get <-function() x
	setinv <-function(inv_matrix) inv_x <<- inv_matrix
	getinv <-function() inv_x
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The cacheSolve function returns the inverse of the matrix
## It checks if the inverse is cached and if present,returns it 
## else the inverse of the matrix is calculated using the solve function  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 inv_x <-x$getinv()
		if(nrow(inv_x)!=0 & ncol(inv_x)!=0){
		message("Getting cached value")
		return(inv_x)
		}
		inp_matrix<-x$get()
		inv_x <-solve(inp_matrix)
		x$setinv(inv_x)
		inv_x
}
