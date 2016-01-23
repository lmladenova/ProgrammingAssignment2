## Caching the Inverse of a Matrix. 
## Matrix inversion is time-consuming computation. The following two functions cache the inverse of a matrix.

## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m_inv<-NULL
        set<-function(y){
                x<<-y
                m_inv<<-NULL
        }
        get<-function() x
        setInverse<-function(solve) m_inv<<-solve
        getInverse<-function() m_inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## This function computes the inverse of the "special" matrix created by the above function. If the inverse has already been calculated then it returns it from the caches. If not, it calculates it and stores it.
cacheSolve<-function(x, ...){
        m_inv<-x$getInverse()
        if(!is.null(m_inv)){
                message("getting cached data")
                return(m_inv)
        }
        mat_inv<-x$get()
        m_inv<-solve(mat_inv, ...)
        x$setInverse(m_inv)
        m_inv
}