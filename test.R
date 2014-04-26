source('cachematrix.R')


## Test 1
x <- solve(rbind(c(1, -1/4), c(-1/4, 1)))

t1y <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
y1 <- cacheSolve(t1y)
y2 <- cacheSolve(t1y)

y1 == y2 
y1 == x


## Test 2
x <- solve(rbind(c(1,2,3), c(4,2,2), c(5,1,7)))

t2y <- makeCacheMatrix(rbind(c(1,2,3), c(4,2,2), c(5,1,7)))
y1 <- cacheSolve(t2y)
y2 <- cacheSolve(t2y)

y1 == y2 
y1 == x


