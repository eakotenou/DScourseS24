# Emilien Akotenou
# Install necessary packages if not already installed
if (!requireNamespace("mice", quietly = TRUE)) {
  install.packages("mice")
}
if (!requireNamespace("modelsummary", quietly = TRUE)) {
  install.packages("modelsummary")
}
if (!requireNamespace("xtable", quietly = TRUE)) {
  install.packages("xtable")
}

# Load required packages
library(mice)
library(modelsummary)
library(xtable)
# Set the working directory to a specific path
setwd("~/DScourseS24/ProblemSets/PS7")

#4- Load wages data
wages <- read.csv("wages.csv")

#5- Drop observations with missing hgc or tenure
wages <- wages[complete.cases(wages[,c("hgc","tenure")]),]

#6-Produce summary table

summary <- summary(wages)

latex_table <- xtable(summary)

print(latex_table, type = "latex")

#7
# Calculate rate of missing log wages
round(mean(is.na(wages$logwage)), 3)

# Listwise deletion (assumes MCAR)
model1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married,
             data = wages)

# Mean imputation
wages$logwage_mean <- ifelse(is.na(wages$logwage), mean(wages$logwage, na.rm=TRUE), wages$logwage)

model2 <- lm(logwage_mean ~ hgc + college + tenure + I(tenure^2) + age + married,
             data = wages)

# Predict missing from observed (assumes MAR)            
wages$logwage_pred <- ifelse(is.na(wages$logwage),
                             predict(model1, newdata=wages[is.na(wages$logwage),]),
                             wages$logwage)

model3 <- lm(logwage_pred ~ hgc + college + tenure + I(tenure^2) + age + married,
             data = wages)

# Multiple imputation with mice            
imp <- mice(wages, method = "pmm", m = 5)

lm_model.4 <- with(imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

pooled_lm_model <- pool(lm_model.4)

modelsummary(pooled_lm_model, output = "latex")
# Create a list of models
models_list <- list("Listwise Deletion" = model1,
                    "Mean Imputation" = model2,
                    "Predict Missing" = model3,
                    "Multiple Imputation" = lm_model.4)

# Combine results into one LaTeX table
summary_table <- modelsummary(models_list,
                              stars = TRUE,
                              gof_omit = "IC|Log|Adj|Pseudo|RMSE",
                              coef_omit = "Intercept",
                              coef_rename = c("hgc" = "Years of School", "college" = "College Degree"))

# Print the LaTeX table
cat(summary_table)
