#Question1

n <- 1000
df <- 4
#samples <- rt(n, df)
x1 <-rt(n, df=df)
s<- seq(min(x1),max(x1),length=100)
par(mfrow=c(1,1))
hist(x1 , probability=T,breaks=100 , main="T-DISTRIBUTION")
lines(dt(s,df)~s , col = 'red' ,lwd=2)
lines(dnorm(s,mean=0,sd=1)~s, col='green' , lwd=3)
#hist(samples, breaks = 'Scott', freq = FALSE)



#Question2












#question3

nn <- 10000
a <- 5
b<- 12
m <- a+b-1
d<- array(0,dim = c(nn))
for(i in 1:nn){
  u <- runif (m,min=0,max=1)
  u_sort <- sort(u,decreasing = FALSE)
  d[i] <- u_sort[a]
}
hist(d,probability =T ,breaks =100 , main = "r_th_Order_statics")
s<- seq(0,1,length=100)
lines(dbeta(s,a,b)~s,col ='red' , lwd =2)


#Question4
data("mtcars")

sample.mean <- mean(mtcars$mpg)
print(sample.mean)

sample.n <- length(mtcars$mpg)
sample.sd <- sd(mtcars$mpg)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha = 0.01
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))

#Question5

#Using cor.test()

# Taking two numeric
# Vectors with same length
x = c(1, 2, 3, 4, 5, 6, 7)
y = c(1, 3, 6, 2, 7, 4, 5)

# Calculating
# Correlation coefficient
# Using cor.test() method
result = cor.test(x, y, method = "pearson")

# Print the result
print(result)






















        

