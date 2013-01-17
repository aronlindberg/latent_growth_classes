setwd("~/Downloads/")
library(lavaan)

# Load and reshape data 
latent.growth.data <- read.csv(file = "LGC_data.csv", header = TRUE)

latent.growth.data.formatted <- reshape(
  latent.growth.data[c("top100_repository_name","month","monthly_end_with")],
  idvar="top100_repository_name",
  timevar="month",
  direction="wide"
)

# Set the column names
names(latent.growth.data.formatted) <- c("repo_name", "t03", "t04", "t05", "t06", "t07", "t08", "t09", "t10", "t11", "t12")

# Drop the name column
type.name <- "repo_name"

subset(latent.growth.data.formatted, select = names(latent.growth.data.formatted) != type.name)

write.csv(latent.growth.data.formatted, file = "output.csv")

# Write output to a text file
sink(file = "growth_curve_fit_statistics.txt", append = TRUE)

# Fitting the linear latent growth curve model 
 
model <- ' 
# intercept and slope with fixed coefficients
 i =~ 1*t03 + 1*t04 + 1*t05 + 1*t06 + 1*t07 + 1*t08 + 1*t09 + 1*t10 + 1*t11 + 1*t12
 s =~ 0*t03 + 1*t04 + 2*t05 + 3*t06 + 4*t07 + 5*t08 + 6*t09 + 7*t10 + 8*t11 + 9*t12
'

linear <- growth(model, data=latent.growth.data.formatted) 
summary(linear, fit.measures = TRUE)

# Fitting the quadratic latent growth curve model 

model <- ' 
# intercept and slope with fixed coefficients
 i =~ 1*t03 + 1*t04 + 1*t05 + 1*t06 + 1*t07 + 1*t08 + 1*t09 + 1*t10 + 1*t11 + 1*t12
 s =~ 0*t03 + 1*t04 + 2*t05 + 3*t06 + 4*t07 + 5*t08 + 6*t09 + 7*t10 + 8*t11 + 9*t12 
 quad =~ 0*t03 + 1*t04 + 4*t05 + 9*t06 + 16*t07 + 25*t08 + 36*t09 + 49*t10 + 64*t11 + 81*t12
'

quadratic <- growth(model, data=latent.growth.data.formatted) 
summary(quadratic, fit.measures = TRUE)

# Fitting the cubic latent growth curve model 

model <- ' 
# intercept and slope with fixed coefficients
 i =~ 1*t03 + 1*t04 + 1*t05 + 1*t06 + 1*t07 + 1*t08 + 1*t09 + 1*t10 + 1*t11 + 1*t12
 s =~ 0*t03 + 1*t04 + 2*t05 + 3*t06 + 4*t07 + 5*t08 + 6*t09 + 7*t10 + 8*t11 + 9*t12 
 quad =~ 0*t03 + 1*t04 + 4*t05 + 9*t06 + 16*t07 + 25*t08 + 36*t09 + 49*t10 + 64*t11 + 81*t12
 cube =~ 0*t03 + 1*t04 + 2^3*t05 + 3^3*t06 + 4^3*t07 + 5^3*t08 + 6^3*t09 + 7^3*t10 + 8^3*t11 + 9^3*t12 
'

cubic <- growth(model, data=latent.growth.data.formatted) 
summary(cubic, fit.measures = TRUE)

# Print covariance matrix
inspect(quadratic,"cov.lv")

# Turn off sink
sink()