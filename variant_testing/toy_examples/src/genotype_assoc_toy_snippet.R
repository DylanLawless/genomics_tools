library(stargazer)
library(dplyr)
# Is disease outcome associated with gene predictor?
# Print the log OR and 95% CI

# outcome = disease outcome
# predictor = genotype

# data ----
sample_ID <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
predictor <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
outcome <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
df <- data.frame(sample_ID, predictor, outcome)

df_tally <- df %>%
  group_by(outcome, predictor) %>%
  tally() %>%
  as.data.frame()

# tally ----
df_tally

# |          |  predictor0 | predictor1 |
# |----------|--------------------|
# | outcome 0 |   14 (A) |   1 (B) |
# | outcome 1 |    5 (C) |  10 (D) |

# 1. log OR "by hand" ----
a <- df_tally[1,3]
b <- df_tally[2,3]
c <- df_tally[3,3]
d <- df_tally[4,3]

# calculate OR
or <- ((a * d)/(b * c))
or
log(or)

# SE ln(OR)
se <- sqrt(1/a + 1/b + 1/c + 1/d)
se

# 95% CI for log(OR)
se_95pc <- log(or) + (1.96 * se)
se_5pc <- log(or) - (1.96 * se)

# make df
df_example <- c(log(or), se_5pc, se_95pc)
names(df_example) <- c("log_OR", "2.5%", "97.5%")
output_1 <- df_example
output_1

# 2. log OR from glm fit coef ----
fit <- glm(outcome ~ predictor, family = "binomial", data = df)
summary(fit)

# log OR from glm fit
glm_log_OR <- as.data.frame( (cbind(coef(fit), confint.default(fit))) )
output_2 <- glm_log_OR
output_2

# OR from glm fit
glm_OR <- as.data.frame( exp(cbind(coef(fit), confint(fit))) )
output_2b <- glm_OR
output_2b

# 3. log OR from glm fit with stargazer table ----
library(stargazer)
# Table with coefficients (log OR, CI) 
output_3 <- stargazer(fit, ci = T, single.row = T, type = "text", ci.level = 0.95)
output_3

# Table with coefficients (OR, CI) 
OR.vector <- exp(fit$coef)
CI.vector <- exp(confint(fit))
p.values <- summary(fit)$coefficients[, 4]

output_3b <- stargazer(fit, coef = list(OR.vector), ci = T, 
          ci.custom = list(CI.vector), p = list(p.values), 
          single.row = T, type = "text")
output_3b

# Compare output ----
output_1 # log OR by hand
output_2 # log OR from glm
output_3 # log OR from glm stargazer table
output_2b # OR from glm
output_3b # OR from glm stargazer table

# log_OR  2.5% CI   97.5% CI  Method
# 3.33    1.04      5.63      log OR by hand
# 3.33    1.04      5.63      log OR from glm 
# 3.33    1.04      5.63      log OR from glm using stargazer table
# 
# OR      2.5% CI   97.5% CI  Method
# 27.99   3.94      586.83    OR from glm
# 28.00   3.94      586.83    OR from glm using stargazer table
