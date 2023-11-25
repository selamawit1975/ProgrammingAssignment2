makeCacheMatrix<-function(x=matrix()){
        ## @x :a square invertible matrix
        #this function creates a special matrix object that can cache its inverse
        inverse<-null
        set=function (Y) {
                x<<-Y
                inv<<-null
                }
        get<-function()x
        setinv <-function(inverse)inv<<-inverse
        getinv<-function()inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)}

cachesolve<-function (x,...){
        ## @x :output for makecachematrix()
        # this function computes the inverse of the special matrix returned by makeCacheMatrix.
        inv<x$getinv()
        if(!isnull(inv)){
                return(inv)
                }
        mat.data=x$get()
        inv=solve(mat.data,...)
        x$setinv(inv)
        return(inv)
        }

