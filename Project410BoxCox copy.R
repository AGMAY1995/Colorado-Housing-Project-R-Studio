project <- read.csv(file="/Users/jorgegonzalez/Downloads/410 stuff/80111.csv",
                    header = TRUE, sep = ",")

#cutting down the price and sqft by ones
price10K <- project$price/10000
sqft1k <- project$sqft/1000

#plotting the data of the cut price & testing the normality of the data
library(rcompanion)
plotNormalHistogram(price10K)
shapiro.test(price10K)
#finding the best value for Lambda
library(MASS)
BoxCox_Project <- boxcox(price10K ~ sqft1k + bedrooms + bathrooms + year, 
                         data = project, lambda = seq(-3, 3, 1/4), interp = FALSE)
BoxCox_data <- data.frame(BoxCox_Project$x, BoxCox_Project$y)
project_ordered <- BoxCox_data[with(BoxCox_data, order(-BoxCox_Project.y)),]
project_ordered[1,]
#applying lambda=0.25
transformed_price <- 2*(sqrt(price10K)-1)
#plotting the transformed histogram response & finding the normality of it
plotNormalHistogram(transformed_price)
shapiro.test(transformed_price)
#fitting the linear model from the transformed response
project_fitted_model <- glm(transformed_price ~ sqft1k + bedrooms + bathrooms + year, 
                     data = project, family = gaussian(link = identity))
summary(project_fitted_model)
#finding the estimated sigma
sigma(project_fitted_model)
#checking the fit of the model
project_null_model <- glm(transformed_price ~ 1, family = gaussian(link = identity))
deviance <- -2 * (logLik(project_null_model) - logLik(project_fitted_model))
print(deviance)
#finding the pvalue of the deviance and printing it 
pvalue <- pchisq(deviance, 4, lower.tail = FALSE)
print(pvalue)

#prediction
#sqft1K = 4, bedrooms = 5, bathrooms = 3, year = 2000
# Using the fitted model
predicted_transformed_price <- predict(project_fitted_model, data.frame(sqft1k = 4, bedrooms = 5, 
                                       bathrooms = 3, year = 2000))
prediction_price <- 10000 * (1 + predicted_transformed_price / 2) ** 2
print(prediction_price)






