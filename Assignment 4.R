#Assignment 4 Question 4
#Tan, Zhenhao (32176232); Thomas, William (25064783); Yan, Chendong (85105328)

#part a
p_1 = 0.5
p_2 = 0.5
p_3 = 0.5
c_1 = c(1, p_1, p_2)
c_2 = c(p_1, 1, p_3)
c_3 = c(p_1, p_3, 1)

B = cbind(c_1, c_2, c_3)

#part b
A = eigen(B)

e = A$values
e = sqrt(e)


C = A$vectors
D = diag(e)

B_sqrt = C %*% D %*% t(C)


print(B_sqrt %*% B_sqrt)
print('the above product is the same as the matrix B')


#part(c)
##create random matrix
z = matrix(rnorm(3000,0,1), nrow = 1000, ncol = 3)
x = z %*% B_sqrt

exp = t(x) %*% x / 1000
print(exp)
#x'x/n would be close to the variance-covariance matrix becuase
#i,j-th element of the expectation of x'x would be expectation of x_i and x_j,
#which is equal to covariance of x_i and x_j assuming that x_i and x_j are iid,
# and of mean 0.

#part d
y = rnorm(1000, 0, 1)
beta_hat = solve(t(x) %*% x) %*% (t(x)%*% y)
print(cat(('beta_hat_1 is ') ,beta_hat[1]))

#part e
x_1 = x[,1]
x_2 = x[,2:3]

m = diag(1000) - x_2 %*% solve(t(x_2) %*% x_2) %*% t(x_2)
beta_hat_1_cal = solve(t(x_1) %*% m %*% x_1) %*% (t(x_1) %*% m %*% y)
print(beta_hat_1_cal)

#part e
beta_hat_1_ols = solve(t(x_1) %*% x_1) %*% (t(x_1)%*% y)
print(beta_hat_1_ols)
#this is different from the values in the earlier parts because 
#simply taking OLS of Y wrt X_1 disregarded the correlation between x_1 
#and x_2, and will make the regression coefficient different from the 
#one where you actually take into account said correlation

#now, if x_1 and x_2 have no correlation then the value produced by 
#ols would be the same as partitioned regression on x_1

c_1 = c(1, 0, 0)
c_2 = c(0, 1, 0.5)
c_3 = c(0, 0.5, 1)
B_new = cbind(c_1, c_2, c_3)

A = eigen(B_new)

e = A$values
e = sqrt(e)


C = A$vectors
D = diag(e)

#produce new B^(1/2)
B_sqrt_new = C %*% D %*% t(C)
x = z %*% B_sqrt_new

x_1 = x[,1]
x_2 = x[,2:3]

m = diag(1000) - x_2 %*% solve(t(x_2) %*% x_2) %*% t(x_2)
beta_hat_1_cal = solve(t(x_1) %*% m %*% x_1) %*% (t(x_1) %*% m %*% y)
print(beta_hat_1_cal)


beta_hat_1_ols = solve(t(x_1) %*% x_1) %*% (t(x_1)%*% y)
print(beta_hat_1_ols)
#verified
