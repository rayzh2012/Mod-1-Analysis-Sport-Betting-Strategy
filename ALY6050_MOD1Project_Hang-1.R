#ALY 6050
#Name: Hang Wu

#The probability that the Red Sox win a
#game in Boston is 0.59

#the probability that Yankees win in Newyork
#game is 0.56
#
Prb = 0.59
Pyn = 0.56
w = 1000
l = -1050

#If the first game is played in New York, the second game is played in Boston, and the
#third game (if it becomes necessary) is in New York, then complete parts (i)-(v) below.

#i)  Calculate the probability that the Red Sox will win the series (win 2 games).

# = prob that R win in game 1 (new york) & 2(Boston) + Prob that R win in game 2(Boston) & 3(New York) + Prov that R win in 1&3
Prw1 = (1-Pyn)*Prb+ Pyn*Prb*(1-Pyn) + (1-Pyn)*(1-Pyn)*(1-Prb)
#0.484352


#ii)Construct a probability distribution for your net win (X) in the series. Calculate your
#expected net win (the mean of X) and the standard deviation of X.

#random Var X: Net Winning
#X4 = 2000 when R win 1&2
#X3 = 950 when R win game 1&3 or game 2&3
#X2 = -1100 when R wins only game 1 or game 2
#X1 = -2100 when R loses both game 1 and game 2
x4 = 2000
x3 = 950
x2 = -1100
x1 = -2100
Px4 = Prb*(1-Pyn)
Px3 = (1-Pyn)*Prb*(1-Pyn) + Pyn*(1-Pyn)*Prb
Px2 = Prb*Pyn*(1-Prb)+(1-Prb)*(1-Pyn)*(1-Prb)
Px1 = (1-Prb)*Pyn
Ex = x1*Px1+x2*Px2+x3*Px3+x4*Px4
Ex
#53.2892
E21 = x1**2*Px1
E22 = x2**2*Px2
E23 = x3**2*Px3
E24 = x4**2*Px4
Vx1 = E21 - (Px1*x1)**2
Vx2 = E22 - (Px2*x2)**2
Vx3 = E23 - (Px3*x3)**2
Vx4 = E24 - (Px4*x4)**2
sdX = (Vx1+Vx2+Vx3+Vx4)**(0.5)
sdX
#1486.07
###

#iii)create 10,000 random values for X. Let these random values be
#denoted by Y. Use these Y values to estimate your expected net win by using a 95%
#confidence interval. Does this confidence interval contain E(X)?
n = 10000
y = sample(c(x1,x2,x3,x4),size=n,replace=T,prob=c(Px1,Px2,Px3,Px4))
m = mean(y)
m
#64.475
std = sd(y)
std
#1619.207
LowerBound = m-1.96*(std/(10000**0.5))
LowerBound
#32.73855
UpperBound = m+1.96*(std/(10000**0.5))
UpperBound
#96.21145
#yes, it's within


#iv) Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit
#test to verify how closely the distribution of Y has estimated the distribution of X.
hist(y,labels=T,
     main="Histogram for Y:Best of 3,Start NY", 
     xlab="Net Winning($)", 
     ylab="Frequency",
     border="blue", 
     col="green",
     ylim=c(0,3500),
     las=1, 
     breaks=5)
summary(y)
X = data.frame(c(Px1,Px2,Px3,Px4),c(x1,x2,x3,x4))
colnames(X)[1] = "Probability"
colnames(X)[2] = "Net Winning"

barplot(height=X$Probability,names.arg=X$`Net Winning`,main="Probability Distribution Graph of X: Start in Ny",ylab='Probability',xlab='Net Winning($)',col="#69b3a2")
#sum(X)


#Degrees of freedom = no. of categories – 1 
#O = observed frequency
#E = expected frequency

#h0: no diff
#h1: diff
c = 4
o = c(2318,2261,2707,2714)/n
sum(o)
X = c(Px1,Px2,Px3,Px4)
sum(X)
x2 = sum(((o-X)**2)/X)
qchisq(x2,df = c-1,lower.tail=FALSE)
#14.44381
qchisq(0.05,df = c-1,lower.tail=FALSE)
#critical = 7.81
#reject H0, significant difference for each category, each scenario is significantly distinguishable

#Part 2 
#i) Repeat part 1 above but assume that the first game is played in
#Boston, the second game is played in New York, and the third
#game (if it becomes necessary) is in Boston.
Prw2 = Prb*(1-Pyn) + (1-Prb)*(1-Pyn)*Prb + Prb*Pyn*Prb

#0.560972

#ii)Construct a probability distribution for your net win (X) in the series. Calculate your
#expected net win (the mean of X) and the standard deviation of X.

#random Var X: Net Winning
#X4 = 2000 when R win 1&2
#X3 = 950 when R win game 1&3 or game 2&3
#X2 = -1100 when R wins only game 1 or game 2
#X1 = -2100 when R loses both game 1 and game 2
x4 = 2000
x3 = 950
x2 = -1100
x1 = -2100
Px4 = Prb*(1-Pyn)
Px3 = Prb*Pyn*Prb + (1-Prb)*(1-Pyn)*Prb
Px2 = Prb*(1-Pyn)*(1-Prb)+(1-Prb)*(1-Pyn)*(1-Prb)
Px1 = (1-Prb)*Pyn
Ex = x1*Px1+x2*Px2+x3*Px3+x4*Px4
Ex
# 124.9034
E21 = x1**2*Px1
E22 = x2**2*Px2
E23 = x3**2*Px3
E24 = x4**2*Px4
Vx1 = E21 - (Px1*x1)**2
Vx2 = E22 - (Px2*x2)**2
Vx3 = E23 - (Px3*x3)**2
Vx4 = E24 - (Px4*x4)**2
sdX = (Vx1+Vx2+Vx3+Vx4)**(0.5)
sdX
#1384.851

#iii)create 10,000 random values for X. Let these random values be
#denoted by Y. Use these Y values to estimate your expected net win by using a 95%
#confidence interval. Does this confidence interval contain E(X)?
n = 10000
#x1 = sample(c(w,l),size = 10000,replace=T,prob=c(Prb,1-Prb))
#x2 = sample(c(w,l),size = 10000,replace=T,prob=c(1-Pyn,Pyn))
#x3 = sample(c(w,l),size = 10000,replace=T,prob=c(Prb,1-Prb))
#y = x1+x2+x3
y = sample(c(x1,x2,x3,x4),size=n,replace=T,prob=c(Px1,Px2,Px3,Px4))
m = mean(y)
m
#144.425
std = sd(y)
std
#1606.242
LowerBound = m-1.96*(std/(10000**0.5))
LowerBound
#112.9427
UpperBound = m+1.96*(std/(10000**0.5))
UpperBound
#175.9073
#yes, it's within

#iv) Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit
#test to verify how closely the distribution of Y has estimated the distribution of X.
hist(y,labels=T,
     main="Histogram for Y:Best of 3,Start Bos", 
     xlab="Net Winning($)", 
     ylab="Frequency",
     border="blue", 
     col="green",
     ylim=c(0,3500),
     las=1, 
     breaks=5)
summary(y)
#Degrees of freedom = no. of categories – 1 
#O = observed frequency
#E = expected frequency
#h0: no diff
#h1: diff
c = 4
o = c(2307,1872,3137,2684)/n
sum(o)
X = data.frame(c(Px1,Px2,Px3,Px4),c(x1,x2,x3,x4))
colnames(X)[1] = "Probability"
colnames(X)[2] = "Net Winning"

barplot(height=X$Probability,names.arg=X$`Net Winning`,main="Probability Distribution Graph of X: Start in Bos",ylab='Probability',xlab='Net Winning($)',col="#69b3a2")
#sum(X)

x2 = sum(((o-X)**2)/X)
qchisq(x2,df = c-1,lower.tail=FALSE)
#16.13454
#reject H0
##################################################################
#Part 3
#Best of 5, win 3
#win 1,2,3;1,2,4;1,2,5;1,3,4;1,3,5;1,4,5;2,3,4;2,3,5;2,4,5;3,4,5 
#total of 5 choose 3 = 10 possible combinations
#total of 4 choose 3 = 4 combinations when we start counting the winning from the second game
# winning 2,4,5 and 3,4,5 habe their own combinations
#1 of 3 variable multiplication, 3 of 4 variables multiplication, and 6 of 5 variable multiplication
a = 1-Pyn
b = Prb
#a = Prb
#b = 1-Pyn

#1)
Pw = a*b*a+2*a*b**2*(1-a)+a**2*(1-b)*b+4*a**2*b*(1-a)*(1-b)+a*b**2*(1-a)**2+a**3*(1-b)**2
Pw
#0.4998545
#2)
x1 = w*3  #winning 1,2,3
x2 = w*3+l#winning 1,2,4;1,3,4;2,3,4, lose one game for these games
x3 = w*3+l*2#lose 2 games out of 5(winning the other 3, played all 5 games)
x4 = l*3 #lose 1,2,3 straight
x5 = w+l*3#lose 1,2,4;1,3,4;2,3,4, lose 3 games out of 4, stopped at the 4th game
x6 = w*2+l*3
Px1 = a*b*a #1 scenario
Px2 = 2*a*b**2*(1-a)+a**2*(1-b)*b #3 scenarios
Px3 = 4*a**2*(1-a)*(1-b)+a*b**2*(1-a)**2+a**3*(1-b)**2 #6 scenarios
a = (1-a)
b = (1-b)
Px4 = a*b*a #1 scenario
Px5 = 2*a*b**2*(1-a)+a**2*(1-b)*b #3 scenarios
Px6 = 4*a**2*(1-a)*(1-b)+a*b**2*(1-a)**2+a**3*(1-b)**2 #6 scenarios

Ex = x1*Px1+x2*Px2+x3*Px3+x4*Px4+x5*Px5+x6*Px6
Ex
#-227.3243
#Ex1=(1-Pyn)*w + (Pyn)*l
#Ex2=Prb*w + (1-Prb)*l
#Ex = 3*Ex1+2*Ex2
#Ex
#-125
#Ex21 = w**2*(1-Pyn)+l**2*(Pyn)
#Ex23 = Ex21
#Ex25 = Ex21
#Ex22 = w**2*Prb+l**2*(1-Prb)
#Ex24=Ex22
#Vx1 = Ex21-Ex1**2
##Vx2 = Ex22-Ex2**2
#Sdx = (3*Vx1+2*Vx2)**(0.5)
#Sdx
#2267.081
E21 = x1**2*Px1
E22 = x2**2*Px2
E23 = x3**2*Px3
E24 = x4**2*Px4
E25 = x5**2*Px5
E26 = x6**2*Px6
Vx1 = E21 - (Px1*x1)**2
Vx2 = E22 - (Px2*x2)**2
Vx3 = E23 - (Px3*x3)**2
Vx4 = E24 - (Px4*x4)**2
Vx5 = E25 - (Px5*x4)**2
Vx6 = E26 - (Px6*x4)**2
sdX = (Vx1+Vx2+Vx3+Vx4+Vx5+Vx6)**(0.5)
sdX
#1731.125
#iii)create 10,000 random values for X. Let these random values be
#denoted by Y. Use these Y values to estimate your expected net win by using a 95%
#confidence interval. Does this confidence interval contain E(X)?
n = 10000
#x2 = sample(c(w,l),size = 10000,replace=T,prob=c(Prb,1-Prb))
#x1 = sample(c(w,l),size = 10000,replace=T,prob=c(1-Pyn,Pyn))
#x3 = sample(c(w,l),size = 10000,replace=T,prob=c(Prb,1-Prb))
#x4 = sample(c(w,l),size = 10000,replace=T,prob=c(1-Pyn,Pyn))
#x5 = sample(c(w,l),size = 10000,replace=T,prob=c(Prb,1-Prb))
y =  sample(c(x1,x2,x3,x4,x5,x6),size = n,replace=T,prob=c(Px1,Px2,Px3,Px4,Px5,Px6))
#y = x1*3+x2*2
m = mean(y)
m
#-42.315
std = sd(y)
std
#1891.626
LowerBound = m-1.96*(std/(10000**0.5))
LowerBound
#-79.39087
UpperBound = m+1.96*(std/(10000**0.5))
UpperBound
#-5.239127
#no not within
#iv) Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit
#test to verify how closely the distribution of Y has estimated the distribution of X.

hist(y,labels=T,
main="Histogram for Y:Best of 5,Start NY", 
xlab="Net Winning($)", 
ylab="Frequency",
border="blue", 
col="green",
ylim=c(0,3500),
las=1, 
breaks=5)

summary(y)

X = data.frame(c(Px1,Px2,Px3,Px4,Px5,Px6),c(x1,x2,x3,x4,x5,x6))
colnames(X)[1] = "Probability"
colnames(X)[2] = "Net Winning"
X = X[order(X$`Net Winning`),]
barplot(height=X$Probability,
        names.arg=X$`Net Winning`,
        main="Probability Distribution Graph of X: Start in NY",
        ylab='Probability',xlab='Net Winning($)',col="#69b3a2" )
#sum(X)

#Degrees of freedom = no. of categories – 1 
#O = observed frequency
#E = expected frequency
#h0: no diff
#h1: diff
c = 6
o = c(1546,1804,2845,2525,1280)/n
sum(o)
X = c(Px1,Px2,Px3,Px4,Px5,Px6)
sum(X)
XX = sum(((o-X)**2)/X)
qchisq(XX,df = c-1,lower.tail=FALSE)
# 7.494385
#can't reject H0

#Part 4
#Best of 7, win 4, 7 choose 4 = 35 possible combinations
a = Prb
b = 1-Pyn
#1
Pw = a**2*b**2+2*a**3*b*(1-b)+3*a**2*b**2*(1-a)+7*a**2*b**2*(1-a)*(1-b)+3*(b**3*a*(1-a)**2)+a**3*b*(1-b)**2*3+b**2*a**2*(1-a)**2*(1-b)*7+a**2*b**2*(1-b)**2*(1-a)*4
Pw
# 0.5492387
#2
x1 = w*4 #win 1,2,3,4
x2 = w*4+l*3 #4 out of 7
x3 = w*4+l*2  #4 out of 6
x4 = w*4+l #4 out of 5
x5 = l*4+w#lose 3 out of 5
x6 = l*4+w*2  #lose 3 out of 6
x7 = l*4+w*3 #lose 3 out of 7
x8 = l*4 #lose 4 straight
Px1 = a*b*a*b #win 1,2,3,4
Px2 = b**2*a**2*(1-a)**2*(1-b)*7 + a**2*b**2*(1-b)**2*(1-a)*4 #win4 out of 7
Px3 = 7*a**2*b**2*(1-a)*(1-b)+3*(b**3*a*(1-a)**2)+a**3*b*(1-b)**2*3 #win 4 out of 6
Px4 = a**2*b**2+2*a**3*b*(1-b)+3*a**2*b**2*(1-a)#win 4 out of 5
a=(1-a) #losing
b=(1-b) #losing
Px5 = a*b*a*b #lose 1,2,3,4
Px6 = b**2*a**2*(1-a)**2*(1-b)*7 + a**2*b**2*(1-b)**2*(1-a)*4 #lose 4 out of 7
Px7 = 7*a**2*b**2*(1-a)*(1-b)+3*(b**3*a*(1-a)**2)+a**3*b*(1-b)**2*3 #lose 4 out of 6
Px8 = a**2*b**2+2*a**3*b*(1-b)+3*a**2*b**2*(1-a)#lose 4 out of 5
Ex = x1*Px1+x2*Px2+x3*Px3+x4*Px4+x5*Px5+x6*Px6+x7*Px7+x8*Px8
Ex #160.1119
E21 = x1**2*Px1
E22 = x2**2*Px2
E23 = x3**2*Px3
E24 = x4**2*Px4
E25 = x5**2*Px5
E26 = x6**2*Px6
E27 = x7**2*Px7
E28 = x8**2*Px8
Vx1 = E21 - (Px1*x1)**2
Vx2 = E22 - (Px2*x2)**2
Vx3 = E23 - (Px3*x3)**2
Vx4 = E24 - (Px4*x4)**2
Vx5 = E25 - (Px5*x5)**2
Vx6 = E26 - (Px6*x6)**2
Vx7 = E27 - (Px7*x7)**2
Vx8 = E28 - (Px8*x8)**2
sdX = (Vx1+Vx2+Vx3+Vx4+Vx5+Vx6+Vx7+Vx8)**(0.5)
sdX #2647.909
y =  sample(c(x1,x2,x3,x4,x5,x6,x7,x8),size = n,replace=T,prob=c(Px1,Px2,Px3,Px4,Px5,Px6,Px7,Px8))
m = mean(y)
m
#94.69742
std = sd(y)
std
#2739.72
LowerBound = m-1.96*(std/(10000**0.5))
LowerBound
#40.9989
UpperBound = m+1.96*(std/(10000**0.5))
UpperBound
#222.5365
#no not within
#iv) Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit
#test to verify how closely the distribution of Y has estimated the distribution of X.
hist(y,labels=T,
     main="Histogram for Y:Best of 7,Start Bos", 
     xlab="Net Winning($)", 
     ylab="Frequency",
     border="blue", 
     col="green",
     ylim=c(0,3500),
     las=1, 
     breaks=5)

summary(y)
X = data.frame(c(Px1,Px2,Px3,Px4,Px5,Px6,Px7,Px8),c(x1,x2,x3,x4,x5,x6,x7,x8))
colnames(X)[1] = "Probability"
colnames(X)[2] = "Net Winning"
X = X[order(X$`Net Winning`),]
barplot(height=X$Probability,
        names.arg=X$`Net Winning`,
        main="Probability Distribution Graph of X: Start in Bos",
        ylab='Probability',xlab='Net Winning($)',col="#69b3a2",las=2 )
#sum(X)
#Degrees of freedom = no. of categories – 1 
#O = observed frequency
#E = expected frequency
#h0: no diff
#h1: diff
c = 8
o = c(1605,451,720,1691,741,1935,2248,609)/n
X = c(Px1,Px2,Px3,Px4,Px5,Px6,Px7,Px8)
XX = sum(((o-X)**2)/X)
qchisq(XX,df = c-1,lower.tail=FALSE)
# 6.176587
#can't reject H0

