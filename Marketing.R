install.packages('pacman', repos = "http://cran.us.r-project.org")
pacman::p_load(tidyverse, caret, corrplot, broom, ggpubr, MASS,relaimpo, car, e1071,interplot)
setwd("~/Dropbox/EBAC/workshop-data/PA/data/bank marketing")

library("readxl")
marketing <- read_xlsx("bank_data.xlsx")

head(marketing)
str(marketing)
summary(marketing)

# set the variables to factors (categorical data)
colnames(marketing)
cols = c('job','marital','education','default','housing','loan','contact','poutcome','y')
marketing[,cols] = lapply(marketing[,cols],as.factor)

# change the base to 1
marketing$y <- factor(marketing$y, levels = c("no","yes"), labels = c(0,1))
contrasts(marketing$y)

options(repr.plot.width=6, repr.plot.height=4)
marketing %>%
  group_by(y) %>%
  summarise(per = n()/nrow(marketing)) %>%
  ggplot(aes(x=y, y=per, fill = y)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = round(per, 2)), vjust = 2)

# Train and Test Split
library(caTools)
set.seed(123)
splitData = sample.split(marketing$y, SplitRatio = 0.7)
train_set = marketing[splitData,]
nrow(train_set)/nrow(marketing)
test_set = marketing[!splitData,]
nrow(test_set)/nrow(marketing)

# 1st Model
model1 = glm(y~.,data = train_set,family = binomial)
summary(model1)
vif(model1)

# refine model1
model2 = step(model1, trace = F)
summary(model2)

# test model2 on train set
trainPredict = predict(model2, newdata = train_set, type = 'response')
threshold <- sum(marketing$y == "1")/length(marketing$y)
predict <- ifelse(trainPredict > 0.5, 1, 0)
matrix_table <- table(train_set$y,predict)
matrix_table

# Accuracy
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)
confusionMatrix(table(predict,train_set$y), positive='1')

# variable importance
varImp(model2)

# test model2 on the test set
testPredict = predict(model2, newdata = test_set, type = 'response')
predict = ifelse(testPredict > 0.5, 1,0)
matrix_table = table(test_set$y, predict)
matrix_table

# Accuracy
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)
confusionMatrix(table(predict, test_set$y), positive='1')

# 3rd model
model3 = glm(formula = y ~ job + education + balance + housing + loan + contact + day + month + duration + campaign + poutcome, 
    family = binomial, data = train_set)
summary(model3)

# test model3 on train set
trainPredict = predict(model3, newdata = train_set, type = 'response')
threshold <- sum(marketing$y == "1")/length(marketing$y)
predict <- ifelse(trainPredict > 0.5, 1, 0)
matrix_table <- table(train_set$y,predict)
matrix_table

# Accuracy
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)
confusionMatrix(table(predict,train_set$y), positive='1')

# variable importance
varImp(model3)

# test model3 on the test set
testPredict = predict(model3, newdata = test_set, type = 'response')
predict = ifelse(testPredict > 0.5, 1,0)
matrix_table = table(test_set$y, predict)
matrix_table

# Accuracy
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)
confusionMatrix(table(predict, test_set$y), positive='1')

# Sorted order of probabilities
head(sort(testPredict, decreasing = T),10)

# Lift Chart
install.packages("ROCR")
library(ROCR)
pred = prediction(trainPredict, train_set$y)
perf = performance(pred, "lift", "rpp")
plot(perf, main="lift curve", xlab = 'Proportion of Customers (sorted prob)')

# p-value for the model
with(model3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=F))
