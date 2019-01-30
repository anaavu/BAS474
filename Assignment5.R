#Inputs
w1_4<- -0.5
w1_5<- -0.2
w2_4<- +0.3
w2_5<- +0.2
w3_4<- +0.4
w3_5<- -0.1
w4_y<- +0.2
w5_y<- +0.4

x1<-2.2
x2<-1.3
x3<-0.7

y<-1
b<-1

lambda<-0.1

#Compute x4,x5 and yhat

z4 <- w1_4*x1 + w2_4*x2 + w3_4*x3 + b
(z4act <- 1/(1+exp(-z4)))

z5 <- w1_5*x1 + w2_5*x2 + w3_5*x3 + b
(z5act <- 1/(1+exp(-z5)))

z <- w4_y*z4act + w5_y*z5act+ b
(y_hat <- 1/(1+exp(-z)))
# Value of y-hat:
# 0.8020925

#Compute the value of the objective function
      
E <- - y*log(y_hat) + (1-y) * log(1-y_hat)
regterm <- (lambda/2) * sum(c(w1_4,w1_5,w2_4,w2_5,w3_4,w3_5,w4_y,w5_y)^2)
E+regterm
# Value of the objective function:
# 0.2600314
