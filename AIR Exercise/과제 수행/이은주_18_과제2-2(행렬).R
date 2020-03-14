#######AIR 동아리 과제2-2(행렬)#######

A <- matrix(c(1,0,0.5,0.2,1,0.7,0.5,0.3,1), nrow=3)
B <- matrix(c(0.6,0.5,0.3), nrow=3, ncol=1)

#1)
2*A; exp(1)*A; sqrt(2)*A
2*B; exp(1)*B; sqrt(2)*B

#2)
crossprod(A,A)

#3)
A%*%B
t(B)%*%A

#4)
B%*%t(B)
t(B)%*%B

#5)
solve(A)*A

#6)
solve(solve(A)%*%A)%*%A%*%B
