install.packages('caret')
install.packages('rsample')
install.packages('vip')
install.packages("glmnet")

library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(vip)
library(tidyr)
library(glmnet)

default_dataset <- read.csv("ds_salaries.csv")

dataset <- default_dataset %>%
  select(-salary, -salary_currency)
dataset$company_location <- as.integer(factor(dataset$company_location))


dataset$remote_ratio <- case_when(
  dataset$remote_ratio %in% c(100, 50) ~ "yes",
  dataset$remote_ratio == 0 ~ "no",
  TRUE ~ "no"
)


### Logistic Regression ###

# dataset <- dataset %>% mutate_if(is.ordered, factor, ordered = F)
dataset$job_title <- as.integer(factor(dataset$company_size))

churn_split <- initial_split(dataset, prop = .7, strata = 'remote_ratio')
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

churn_train$remote_ratio <- factor(churn_train$remote_ratio, levels = c("no", "yes"))

model1 <- glm(remote_ratio ~ work_year, family = 'binomial', data = churn_train)
model2 <- glm(remote_ratio ~ company_size, family = 'binomial', data = churn_train)

tidy(model1)
tidy(model2)

exp(coef(model1))
exp(coef(model2))

# Multiple logistic regression
model3 <- glm(
  remote_ratio ~ work_year + company_size, family = 'binomial',
  data = churn_train
)
tidy(model3)


### ASSESSING MODEL ACCURACY ###

set.seed(123)
cv_model1 <- train(
  remote_ratio ~ work_year,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

 set.seed(123) 
cv_model2 <- train(
  remote_ratio ~ work_year + company_size, 
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model3 <- train(
  remote_ratio ~ .,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

summary( 
  resamples(
    list(
      model1 = cv_model1, model2 = cv_model2, model3 = cv_model3
    ))
)$statistics$Accuracy

pred_class <- predict(cv_model3, churn_train)
levels(pred_class)

# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = 'yes'),
  reference = relevel(churn_train$remote_ratio, ref = 'yes')
)

levels(churn_train$remote_ratio)

library(ROCR)

m1_prob <- predict(cv_model1, churn_train, type = 'prob')$yes
m3_prob <- predict(cv_model3, churn_train, type = 'prob')$yes

perf1 <- prediction(m1_prob, churn_train$remote_ratio) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

perf2 <- prediction(m3_prob, churn_train$remote_ratio) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('cv_model1', 'cv_model3'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)

### Feature interpretation
vip(cv_model3, num_features = 20)

