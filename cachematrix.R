## The square matrix object is created by the function "makeCacheMatrix" for example, x<-makeCacheMatrix(matrix(1:4,4,4)) . after that the "cacheSolve"
## function take the matrix object and calculate its inverse , if the inverse is already present then it returns the inverse otherwise it will calculate
## the inverse and then display it.


makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
        setmatrix<-function(y){
                x<<-y
                inverse<<-NULL
        }
        getmatrix<-function() x
        getinverse_matrix<-function() inverse
        setinverse_matrix<-function(inv){
                inverse<<-inv
        }
        list(setmatrix = setmatrix, getmatrix=getmatrix,getinverse_matrix=getinverse_matrix,setinverse_matrix=setinverse_matrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<- x$getinverse_matrix()
        if(!is.null(inv)) {
                print("getting cached inverse of matrix")
                return(inv)
        }
        inv_calculation<-solve(x$getmatrix())
        x$setinverse_matrix(inv_calculation)
        inv_calculation
}
