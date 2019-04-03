#Importing dataset
bike = read.csv((E:/AML - BUAN 6341/hour.csv',  header=TRUE))

bike = subset(bike, select = -c(instant,dteday,casual,registered,yr,mnth,season,weathersit,holiday,weekday,hum,atemp,windspeed) ) 
bike
##### ADDING DUMMY VARIABLES 

for(level in unique(bike$hr)){
  bike[paste("hr", level, sep = "_")] <- ifelse(bike$hr == level, 1, 0)
}
ncol(bike)
head(bike)
#### SPLITTING THE DATA INTO TRAINING AND TESTING SETS ###########
sample <- sample.int(n = nrow(bike), size = .70*nrow(bike), replace = F)
train <- bike[sample, ]
test  <- bike[-sample, ]
train_y = train['cnt']
train_x = subset(train, select = -c(cnt) )
test_y = test['cnt']
test_x = subset(test, select = -c(cnt) )

betas <- runif(27, min=0, max=1)
betas
train_x <- t(as.matrix(train_x))
train_y <- t(as.matrix(train_y))
test_x <- t(as.matrix(test_x))
test_y <- t(as.matrix(test_y))
beta   <- matrix(betas,nrow=1,ncol=27)

## Cost Function
cost = function(theta){
  return(1/(2*ncol(train_x)) * sum(((theta %*% train_x) - train_y)^2))
}

#######Gradient Descent
graddescent = function(alpha, threshold){
  J = cost(beta)
  temp = beta - t(alpha * (train_x %*% t((beta %*% train_x )-train_y)) / ncol(train_x))
  assign('beta',temp,envir=.GlobalEnv)
  new_J = cost(beta)
  delta = abs((J - new_J) / J)
  vec = c(J,new_J)
  J = new_J
  iter = c(0,1)
  i = 1
  while(is.finite(new_J) && (delta > threshold)){
    te = beta - t(alpha * (train_x %*% t((beta %*% train_x )-train_y)) / ncol(train_x))
    assign('beta',te,envir=.GlobalEnv)
    new_J = cost(beta)
    delta = abs((J - new_J) / J)
    i = i+1
    vec = c(vec,new_J)
    iter = c(iter,i)
    J = new_J
    
  }
  assign('coeff',t(beta),envir=.GlobalEnv)
  assign('J_values',t(vec),envir=.GlobalEnv)
  assign('iteration',t(iter),envir=.GlobalEnv)
  print(J)
}

graddescent(alpha = 0.01,threshold = 0.0000001)
##Run this only for the train data
plot(iteration, J_values, ylab="Cost", xlab="iteration", main="Cost Function")

#####################Test Data##############

graddescent_test = function(alpha, threshold){
  J = cost(beta)
  temp = beta - t(alpha * (test_x %*% t((beta %*% test_x )-test_y)) / ncol(test_x))
  assign('beta',temp,envir=.GlobalEnv)
  new_J = cost(beta)
  delta = abs((J - new_J) / J)
  vec = c(J,new_J)
  J = new_J
  iter = c(0,1)
  i = 1
  while(is.finite(new_J) && (delta > threshold)){
    te = beta - t(alpha * (test_x %*% t((beta %*% test_x )-test_y)) / ncol(test_x))
    assign('beta',te,envir=.GlobalEnv)
    new_J = cost(beta)
    delta = abs((J - new_J) / J)
    i = i+1
    vec = c(vec,new_J)
    iter = c(iter,i)
    J = new_J
    
  }
  assign('coeff',t(beta),envir=.GlobalEnv)
  assign('J_values',t(vec),envir=.GlobalEnv)
  assign('iteration',t(iter),envir=.GlobalEnv)
  print(J)
}

graddescent_test(alpha = 0.011,threshold = 0.000001) 
##Run this only for the test data
plot(iteration, J_values, ylab="Cost", xlab="iteration", main="Cost Function")
