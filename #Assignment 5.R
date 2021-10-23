#Assignment 5 
#Tan, Zhenhao; Thomas, William; Yan, Chengdong

#set up the problem

## the variance-covariance matrix is given in the question, we will 
## generate the regressors in the same manner as in Assignement 4,Q4

##generate the variance-covariance matrix 




## Question 2
library(nlme)
library(foreign)
### part(a)
c_1 = c(1,0.9)
c_2 = c(0.9,1)

B = cbind(c_1,c_2)

A = eigen(B)
e = A$values
e = sqrt(e)

C = A$vectors
D = diag(e)

B_sqrt = C %*% D %*% t(C)



z = matrix(rnorm(200,0,1), nrow = 100, ncol = 2)
X = z %*% B_sqrt
X_1 = X[,1]
X_2 = X[,2]
#generate U
U = matrix(rnorm(100,0,1), nrow = 100, ncol = 1)
Y = X_1 + X_2 + U
### part(b)
#running the reg of y against x1, x2
m_1 = lm(Y ~ X)
tols = m_1$coefficients['X1']
print(tols - 1)

m_2 = lm(Y ~ X_1)
ols = m_2$coefficients['X_1']
print(ols - 1)


y_tols = X_1 * tols
y_ols = X_1 * ols

tols_se = sqrt(t(Y - y_tols) %*% (Y - y_tols)) / (100-2) * sqrt(solve((t(X_1) %*% X_1))[1,1])
ols_se = sqrt(t(Y - y_ols) %*% (Y - y_ols)) / (100-2) * sqrt(solve((t(X_1) %*% X_1))[1,1])

print(tols_se)
print(ols_se)




### part(c)
vtols_error = NULL
vols_error = NULL
v_tols_se = NULL
v_ols_se = NULL

R = 10^3
for (i in (1:R)) {
z = matrix(rnorm(200,0,1), nrow = 100, ncol = 2)
X = z %*% B_sqrt
X_1 = X[,1]
X_2 = X[,2]
#generate U
U = matrix(rnorm(100,0,1), nrow = 100, ncol = 1)
Y = X_1 + X_2 + U
### part(b)
####running reg of y against x1, x2
m_1 = lm(Y ~ X)
tols = m_1$coefficients['X1']
vtols_error = cbind(vtols_error,tols - 1)

####running reg of y against x1 only
m_2 = lm(Y ~ X_1)
ols = m_2$coefficients['X_1']
vols_error = cbind(vols_error, ols - 1)


#standard errors
y_tols = X_1 * tols
y_ols = X_1 * ols

tols_se = sqrt(t(Y - y_tols) %*% (Y - y_tols)) / (100-2) * sqrt(solve((t(X_1) %*% X_1))[1,1])
ols_se = sqrt(t(Y - y_ols) %*% (Y - y_ols)) / (100-2) * sqrt(solve((t(X_1) %*% X_1))[1,1])

v_tols_se = cbind(v_tols_se, tols_se)
v_ols_se = cbind(v_ols_se, ols_se)}


print(paste0(('The mean bias of the regresssion of Y against X_1 is'),mean(vtols_error)))

cat(('The mean bias of the regression of Y agaginst X_2 is '),mean(vols_error) ) 

cat('\n')

print(paste0(('The mean bias of the regresssion of Y against X is'), mean(v_tols_se)))

cat(('The mean bias of the regression of Y agaginst X_1 is '), mean(v_ols_se))

##Question 3
read.dta (NEW7080.dta)




