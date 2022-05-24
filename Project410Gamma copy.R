project <- read.csv(file="/Users/jorgegonzalez/Downloads/410 stuff/80111.csv",
                    header = TRUE, sep = ",")

#cutting down the price and sqft by ones
price10K <- project$price/10000
sqft1k <- project$sqft/1000

#fitting the gamma regression
project_fitted_model <- glm(price10K ~ sqft1k + bedrooms + bathrooms + year, data = project, 
                    family = Gamma(link = log))
summary(project_fitted_model)

#checking the fit of the model
project_null_model <- glm(price10K ~ 1, family = Gamma(link = log))
print(deviance <- -2*(logLik(project_null_model)-logLik(project_fitted_model)))

#deviance p-value
print(pvalue <- pchisq(deviance, df = 4, lower.tail = FALSE))

#prediction
print(10000*predict(project_fitted_model, type = "response", data.frame(sqft1k = 4, 
                                        bedrooms = 5, bathrooms = 3, year = 2000)))



