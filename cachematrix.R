## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix gets an argument 'h' which is set to be a matrix
## it is tasked to return a list containing four functions 
## which will be the input of the cacheSolve() function:
##      set(): set the matrix
##      get(): get the matrix
##      set_m_inv: set the matrix inverse
##      get_m_inv: get the matrix inverse

makeCacheMatrix <- function(h = matrix()){
        m_inv = NULL
        set = function(r) { 
                h <<- r
                m_inv=NULL
        }
        ## using <<- to assign an object in an environment
        ## that is different from the current environment
        get = function() h
        set_m_inv = function(inverse) m_inv <<- inverse
        get_m_inv = function() m_inv
        list(set=set, get=get, set_m_inv=set_m_inv, get_m_inv=get_m_inv)
        ## set the matrix
        ## get the matrix
        ## set the inverse of the matrix
        ## get the inverse of the matrix
}


## cacheSolve takes an argument 'h' which is the output
## of the makeCacheMatrix(). 
## output of this function() is the inverse of the original matrix 'h'

cacheSolve <- function(h, ...){
        m_inv = h$get_m_inv()
        ## this if-else block checks if the inverse has
        ## already been calculated
        if(!is.null(m_inv)){
                message("retrieving...")
                return(m_inv)
        }
        else {
                h.data = h$get()
                m_inv = solve(h.data, ...)
        }
        h$set_m_inv(m_inv) ##sets the new value of the inverse in the cache
        return(m_inv) ## returns the value of the inverse
}
