url = "http://www.randomservices.org/random/data/Challenger2.txt"
oring=read.table(url,header=T)
attach(oring)
# attach command is to lable the data
# 
plot(T,I)
oring.lm = lm (I ~T)
# if you would like to see the regression details, you could use the summary function
summary(oring.lm)


# add fitted line to scatterplot
lines(T,fitted(oring.lm))


#From summary data, we could see that the slope value is Estimate -0.24337
# Std.Error is 0.06349
# 95% posterior interval for the slope
-0.24337 - 0.06349*qt(.975,21)
-0.24337 + 0.06349*qt(.975,21)


# note that these are the same as the frequentist confidence intervals
# the Challenger launch was at 31 degrees Fahrenheit
# how much o-ring damage would we predict?
# y-hat
18.36508-0.24337*31

coef(oring.lm)
# use the coefficience to calculate a certain value
coef(oring.lm)[1] + coef(oring.lm)[2]*31  



# posterior prediction interval (same as frequentist)
#        fit      lwr      upr
#1 10.82052    4.048269    17.59276
#  we got the fitted values and the 95% intervals with upper and lower values.


predict(oring.lm,data.frame(T=31),interval="predict")  


# Varify this by hand
10.82052-2.102*qt(.975,21)*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))

# posterior probability that damage index is greater than zero
1-pt((0-10.82052)/(2.102*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))),21)












# Galton's seminal data on predicting the height of children from the heights of the parents, all in inches
url1 = "http://www.randomservices.org/random/data/Galton.txt"
heights=read.table(url1,header=T)
attach(heights)
names(heights)
pairs(heights)
summary(lm(Height~Father+Mother+Gender+Kids))
# From the results, we saw the Estimate value of Kids is -0.04382, this value is near the Std.Error in size, and contains zero.
# Therefore, we came to the conclusion that we could regression without the kids as a factor.

summary(lm(Height~Father+Mother+Gender))
# Now all the variables have strong effects

heights.lm=lm(Height~Father+Mother+Gender)



# each extra inch taller a father is is correlated with 0.4 inch extra height in the child

# each extra inch taller a mother is is correlated with 0.3 inch extra height in the child

# a male child is on average 5.2 inches taller than a female child

# 95% posterior interval for the the difference in height by gender

5.226 - 0.144*qt(.975,894)
5.226 + 0.144*qt(.975,894)

# posterior prediction interval (same as frequentist)
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="M"),interval="predict")
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="F"),interval="predict")


