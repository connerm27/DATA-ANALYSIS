# Conner Montgomery
# Final
# R Script


#Clean Data ( name columns )
colnames(iris) = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species")


# Question 2
#Creates model, then plots it
model_s_width = lm(iris$Sepal_Length ~ iris$Sepal_Width, data=iris)
model_p_length = lm(iris$Sepal_Length ~ iris$Petal_Length, data=iris)
model_p_width = lm(iris$Sepal_Length ~ iris$Petal_Width, data=iris)

plot(iris$Sepal_Length ~ iris$Sepal_Width, data=iris)
abline(model_s_width, lwd=3, col = "darkorange")

plot(iris$Sepal_Length ~ iris$Petal_Length, data=iris)
abline(model_p_length, lwd=3, col = "darkorange")

plot(iris$Sepal_Length ~ iris$Petal_Width, data=iris)
abline(model_p_width, lwd=3, col = "darkorange")

# Making Data Cleaner for Multiple Linear Regression Model
Sepal_Length = iris$Sepal_Length
Sepal_Width = iris$Sepal_Width
Petal_Length = iris$Petal_Length
Petal_Width = iris$Petal_Width

# Question 3: MLR
#Multiple Linear Regression Model
s_length_model = lm(Sepal_Length ~ Sepal_Width + Petal_Length + Petal_Width, data = iris)

# Question 4: Anova
anova(s_length_model)

# Question 5: Petal_Width Predictor 
model_info = summary(s_length_model)$coefficients

# Question 6: Confidence Interval
confint(s_length_model, level = 0.90)

# Question 1
plot(fitted(s_length_model), resid(s_length_model), col = "grey", pch = 20, xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h=0, col = "darkorange", lwd = 2)

bptest(s_length_model)


# Question 2
qqnorm(resid(s_length_model), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(s_length_model), col = "dodgerblue", lwd = 2)

shapiro.test(resid(s_length_model))

# Question 3 Cooke's Distance

rstandard(s_length_model)[abs(rstandard(s_length_model)) > 2]

cooks.distance(s_length_model)[85] > 4 / length(cooks.distance(s_length_model))
cooks.distance(s_length_model)[107] > 4 / length(cooks.distance(s_length_model))
cooks.distance(s_length_model)[135] > 4 / length(cooks.distance(s_length_model))
cooks.distance(s_length_model)[136] > 4 / length(cooks.distance(s_length_model))
cooks.distance(s_length_model)[142] > 4 / length(cooks.distance(s_length_model))







