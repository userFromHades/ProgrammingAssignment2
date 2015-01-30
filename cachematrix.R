

## Сreate special type for mathix that can cash the inverse operator

makeCacheMatrix <- function(original = matrix()) {
    inverse <- NULL

    set <- function(x) {
        original <<- x
        inverse  <<- NULL
    }
    get    <- function() original
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse

    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
} 

## calculate inversion or find cashe for matrix

cacheSolve <- function(x) {
    inverse <- x$getInv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    origin <- x$get()

    inverse = solve(origin)
    x$setInv(inverse) 
    inverse 
}
#Test
#library("Matrix", lib.loc="/usr/lib/R/library")
#h = Hilbert(4)
#m = makeCacheMatrix(h)
#m_ = cacheSolve(m)
#m_ = cacheSolve(m)
#m_ %*% h
