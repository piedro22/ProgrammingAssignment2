## These functions intend to cache the inverse of a matrix inside of a special vector that also contains the original matrix. 
## This way, everytime i need to use the inverse matrix again, I dont need to ask the software to recalculate it. 
## It simply returns it from the cache.

## This first function creates a special vector that contains the objects om and im and functions that work with this objects. 
## These functions assign to them the values of the original matrix (om) and its inverse matrix (im), and can afterwards recall them.
## Therefore, this function creates the enviroment where we are going to cache the value of the inverse matrix to recall it whenver needed.
makeCacheMatrix <- function(om = matrix()) {
        im <- NULL
        setom <- function(x) {
                om <<- x
                im <<- NULL
        }
        getom <- function() om
        setim <- function(invmat) {
                im <<- invmat
        }
        getim <- function() im
        list(setom = setom, getom = getom, setim = setim, getim = getim)
}


## This second function is used de recall the cached value of the inverse matrix, if it isnt NULL, or to calculate it through the Solve() function, if it is NULL. 
## Either way, it will give us the value of the inverse matrix that we desire. 

cacheSolve <- function(x, ...) {
        im <- x$getim()
        if(!is.null(im)) {
                message("getting cached inverse matrix")
                return(im)
        }
        else {
                orgmat <- x$getom()
                invmat <- solve(orgmat)
                x$setim(invmat)
                invmat
        }
}
