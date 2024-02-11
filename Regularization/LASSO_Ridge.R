library(ISLR)
library(ggplot2)
library(kableExtra)
library(flextable)
library(caret)
library(glmnet)
library(Metrics)
library(officer)
#Changing the Directory location
setwd("C:/Users/XYZ")

ft2 <- flextable(data.frame(head(College)[0:5]))
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7), type = "continuous", page_margins = page_mar()
)
# save and hand manipulate the table into place
save_as_docx(ft2, path = "describe.docx", pr_section = sect_properties)
#======================================Q1. Splitting the College Data Set in Train Test data=================================
set.seed(123)
train_index <- sample(1:nrow(College),0.70*nrow(College))
train_data <- College[train_index,]
test_data <- College[-train_index,]

train_x <- model.matrix(Private ~.,train_data)[,-1]
test_x <- model.matrix(Private ~., test_data)[,-1]

train_y <- as.numeric(train_data$Private)
test_y <- as.numeric(test_data$Private)

#=====================Q2. Show the results of Ridge Regression and provide an interpretation=========================================
set.seed(123)
cvRidge <- cv.glmnet(train_x, train_y,alpha = 0, nfolds = 10)

#Optimal value of lambda; minimized the prediction error
#Minimum value of lambda - minimizes out of sample loss
#lambda 1SE - minimizes the value of lambda within 1 standard error of lambda
cvRidge$lambda.min
log(cvRidge$lambda.min)
log(cvRidge$lambda.1se)
ft3 <- flextable(data.frame("LamdaMinimum" = log(cvRidge$lambda.min), "SquaredError" = log(cvRidge$lambda.1se)))
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7), type = "continuous", page_margins = page_mar()
)
# save and hand manipulate the table into place
save_as_docx(ft3, path = "describe.docx", pr_section = sect_properties)
#=========================Q3. Plot the graph of Regression function using Ridge Regularization=============================
plot(cvRidge)
#======================Q4. Fit a Ridge regression model against the training set==========================================

#Fitting the final model on the training data using minimum lambda min.
#alpha = 1 for LASSO (L1)
#alpha = 0 for Ridge (L2)
modelMinRidge <- glmnet(train_x, train_y, alpha = 0, lambda = cvRidge$lambda.min)

df <- data.frame(modelMinRidge$a0, modelMinRidge$df, modelMinRidge$dev.ratio)
colnames(df) <- c("a0", "dof", "dev.ratio")

ft4 <- flextable(df)
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7), type = "continuous", page_margins = page_mar()
)

#save and hand manipulate the table into place
save_as_docx(ft4, path = "describe.docx", pr_section = sect_properties)

#Writing Beta Matrix in CSV file
write.csv(as.data.frame(modelMinRidge$beta[,1]), file = 'matrix.csv')

#Displaying regression coefficients
coef(modelMinRidge)

#Writing Beta Matrix in CSV file
write.csv(as.data.frame(coef(modelMinRidge)[,1]), file = 'matrix.csv')
#================================Q5. Determine Root Mean Square Error (RMSE) of Training Set==============================================
modelMinRidge <- glmnet(train_x, train_y, alpha = 0, lambda = cvRidge$lambda.1se)
predsRidgeTrain <- predict(modelRidge1se, newx = train_x)
predsRidgeTrain
trainRidgeRMSE <- rmse(train_y, predsRidgeTrain)
trainRidgeRMSE

#================================Q6. Determine Root Mean Square Error (RMSE) of Test Set==============================================
predsRidgeTest <- predict(modelRidge1se, newx = test_x)
testRidgeRMSE <- rmse(test_y, predsRidgeTest)
testRidgeRMSE

#=====================Q7. Show the results of Lasso Regression and provide an interpretation=========================================
set.seed(123)
cvLasso <- cv.glmnet(train_x, train_y, nfolds = 10)
#Optimal value of lambda; minimized the prediction error
#Minimum value of lambda - minimizes out of sample loss
#lambda 1se - minimizes the value of lambda within 1 standard error of lambda
log(cvLasso$lambda.min)
log(cvLasso$lambda.1se)
cvLasso$lambda.min
ft6 <- flextable(data.frame("LamdaMinimum" = log(cvLasso$lambda.min), "SquaredError" = log(cvLasso$lambda.1se)))
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7), type = "continuous", page_margins = page_mar()
)
# save and hand manipulate the table into place
save_as_docx(ft6, path = "describe.docx", pr_section = sect_properties)
#=====================Q8. lot the graph of Regression function using LASSO Regularization=========================================
plot(cvLasso)
#======================Q9. Fit a LASSO regression model against the training set==========================================
#Fitting the final model on the training data using minimum lambda min.
#alpha = 1 for LASSO (L1)
#alpha = 0 for Ridge (L2)
modelMin <- glmnet(train_x, train_y, alpha = 1, lambda = cvLasso$lambda.min)
modelMin

df2 <- data.frame(modelMin$a0, modelMin$df, modelMin$dev.ratio)
colnames(df2) <- c("a0", "dof", "dev.ratio")

ft7 <- flextable(df2)
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7), type = "continuous", page_margins = page_mar()
)

#save and hand manipulate the table into place
save_as_docx(ft7, path = "describe.docx", pr_section = sect_properties)

modelMin$beta[,1]
#Writing Beta Matrix in CSV file
write.csv(as.data.frame(modelMin$beta[,1]), file = 'matrix.csv')

#Displaying regression coefficients
coef(modelMin)

#Writing Beta Matrix in CSV file
write.csv(as.data.frame(coef(modelMin)[,1]), file = 'matrix.csv')
#=============Q10. Determine Root Mean Square Error (RMSE) of Training Set after applying LASSO Regularization==============================================
model1se <- glmnet(train_x, train_y, alpha = 1, lambda = cvLasso$lambda.1se)
predsTrain <- predict(model1se, newx = train_x)
trainRMSE <- rmse(train_y, predsTrain)
trainRMSE
#=============Q11. Determine Root Mean Square Error (RMSE) of Test Set after applying LASSO Regularization==============================================
predsTest <- predict(model1se, newx = test_x)
testRMSE <- rmse(test_y, predsTest)
testRMSE

#Empty the variables
rm(list = ls())

#Removing plots
dev.off()
