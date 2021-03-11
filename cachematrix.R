## Assignment: Caching the Inverse of a Matrix
## Author: Puneet Singla
## Problem statement: Write a pair of functions that can cache the inverse of a matrix, specifically a square matrix.

## This R script has two functions 1) makeCacheMatrix 2) cacheSolve:
## makeCacheMatrix creates a "special" matrix object.
## cacheSolve checks if the inverse of matrix already exists. If it doesn't, then it calculates the inverse
## and caches it.
## Note: I have built this code leveraging the example: Caching the Mean of a Vector provided in the assignment.

## How to use this?
## Step 1: Run makeCacheMatrix function to assign list of functions to a variable
## Step 2: Run Set function to assign a matrix to the "special" matrix object
## Step 3: Run cacheSolve function to fetch inverse OR calculate inverse for a given matrix

## makeCacheMatrix defines a list of functions to set and get the "special" matrix and its inverse. 

makeCacheMatrix <- function() {
  
  ## Initialize the variables
  x = matrix()
  inv <- NULL
  
  ## Set function performs two activities
  ## 1) Validate if y is a valid square matrix
  ## 2) Set x = y
  
  set <- function (y) {
    if(!is.matrix(y)) return("ERROR: must be a square matrix with no NA/NaN values")
    if(!nrow(y) == ncol(y)) return("ERROR: must be a square matrix with no NA/NaN values")
    if(is.na(det(y))) return("ERROR: must be a square matrix with no NA/NaN values")
    
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of x 
  get <- function() x
  
  ## set the value of inverse of x
  setsolve <- function(solve) inv <<- solve
  
  ## get the value of inverse of x
  getsolve <- function() inv
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cachekSolve checks if the inverse of x exists. If no, then it calculates the inverse of x

cacheSolve <- function(x, ...) {
  
  ## Collect all "special" matrices in z
  z <- list(x,...)
  
  ## For each "special" matrix x in z, check if inverse of matrix already exists else calculate inverse
  for(i in 1:length(z)) {
    
    ## get inverse of x from cache
    inv <- z[[i]]$getsolve()
    
    ## If inverse of x exist in cache, then return that value
    if(!is.null(inv)) {
      message("getting cached data")
      print(inv)
      next()
    }
    
    ## If inverse of x doesn't exist in cache, then calculate the inverse of x
    data <- z[[i]]$get()
    inv <- solve(data)
    
    ## Set inverse of x in parent environment
    z[[i]]$setsolve(inv)
    
    ## Print inverse of x
    print(inv)
  }
  
}