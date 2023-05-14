

# Fun fun so fun thesis =) ------------------------------------------------

# Import data -------------------------------------------------------------

library(tidyverse)
library(scales)
library(readr)
library(ggplot2)
library(dplyr)


df <- read_delim("Employee Attrition database.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- na.omit(df)


# change data class of variables ------------------------------------------


df$Attrition <- as.factor(df$Attrition)
df$OverTime <- as.logical(df$OverTime)
df$StockOptions <- as.logical(df$StockOptionLevel)
df$BusinessTravel <- as.factor(df$BusinessTravel)
df$Department <- as.factor(df$Department)
df$Gender <- as.factor(df$Gender)
df$EducationField <- as.factor(df$EducationField)
df$JobRole <- as.factor(df$JobRole)
df$MaritalStatus <- as.factor(df$MaritalStatus)


df %>% glimpse()
df <- df[,-(21:22)]


# outlier detection and removal -------------------------------------------


options(scipen=999)

chart1 <- df %>% 
 keep(is.numeric) %>%
 pivot_longer(everything()) %>% 
 ggplot(aes(value)) + 
 geom_boxplot() +
 labs (x = "") +
 facet_wrap(~ name, scales = "free")

chart2 <- df %>% 
 keep(is.numeric) %>%
 pivot_longer(everything()) %>% 
 ggplot(aes(value)) + 
 geom_histogram() +
 labs (x = "") +
 facet_wrap(~ name, scales = "free")


df$MonthlyIncome <- ifelse(df$MonthlyIncome %in% boxplot(df$MonthlyIncome, plot = F)$out,
              NA, df$MonthlyIncome)

df$DistanceFromHome <- ifelse(df$DistanceFromHome %in% boxplot(df$DistanceFromHome, plot = F)$out,
         NA, df$DistanceFromHome)

df$NumCompaniesWorked <- ifelse(df$NumCompaniesWorked %in% boxplot(df$NumCompaniesWorked, plot = F)$out,
          NA, df$NumCompaniesWorked)

df$PercentSalaryHike <- ifelse(df$PercentSalaryHike %in% boxplot(df$PercentSalaryHike, plot = F)$out,
            NA, df$PercentSalaryHike)

df$TotalWorkingYears <- ifelse(df$TotalWorkingYears %in% boxplot(df$TotalWorkingYears, plot = F)$out,
             NA, df$TotalWorkingYears)

df$YearsAtCompany <- ifelse(df$YearsAtCompany %in% boxplot(df$YearsAtCompany, plot = F)$out,
             NA, df$YearsAtCompany)

df$YearsInCurrentRole <- ifelse(df$YearsInCurrentRole %in% boxplot(df$YearsInCurrentRole, plot = F)$out,
             NA, df$YearsInCurrentRole)

df$YearsSinceLastPromotion <- ifelse(df$YearsSinceLastPromotion %in% boxplot(df$YearsSinceLastPromotion, plot = F)$out,
                NA, df$YearsSinceLastPromotion)

df$YearsWithCurrManager <- ifelse(df$YearsWithCurrManager %in% boxplot(df$YearsWithCurrManager, plot = F)$out,
                   NA, df$YearsWithCurrManager)

df <- na.omit(df)


# Sample imbalance --------------------------------------------------------

count_attrition <- df %>%
 group_by(Attrition) %>%
 summarize(count = n())

percentage_attrition <- df %>% 
 group_by(Attrition) %>% 
 summarize(percent = 100 * n() / nrow(df))

data_imbalance <- cbind.data.frame(count_attrition,percentage_attrition)

data_imbalance <- data_imbalance[,-3]


# histograms after outlier removal ----------------------------------------


chart4 <- df %>% 
 keep(is.numeric) %>%
 pivot_longer(everything()) %>% 
 ggplot(aes(value)) + 
 geom_boxplot() +
 labs (x = "") +
 facet_wrap(~ name, scales = "free")

chart5 <- df %>% 
 keep(is.numeric) %>%
 pivot_longer(everything()) %>% 
 ggplot(aes(value)) + 
 geom_histogram() +
 labs (x = "") +
 facet_wrap(~ name, scales = "free")


# splitting sample --------------------------------------------------------

set.seed(1)
df$id <- 1:nrow(df)
train <- df %>% dplyr::sample_frac(.7)
test <- dplyr::anti_join(df, train, by = 'id')
test <- test [,-29]
train <- train [,-29]

# Simple analysis ---------------------------------------------------------

#bar charts of categorical features by y

chart6 <- ggplot(df, aes(y = Department)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")

chart7 <- ggplot(df, aes(y = BusinessTravel)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")

chart8 <- ggplot(df, aes(y = EducationField)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")

chart9 <- ggplot(df, aes(y = Gender)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")

chart10 <- ggplot(df, aes(y = JobRole)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")

chart11 <- ggplot(df, aes(y = MaritalStatus)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")

chart12 <- ggplot(df, aes(y = OverTime)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")

chart13 <- ggplot(df, aes(y = StockOptions)) +
  geom_bar(aes(fill = Attrition)) +
  scale_fill_grey(start = 0.6, end = 0.1) +
  theme(legend.position = "right")


library(ggpubr)


combined_plot <- ggarrange(chart6,
                           chart7,
                           chart8,
                           chart9,
                           chart10,
                           chart11,
                           chart12,
                           chart13,
                           nrow = 2,
                           ncol = 4)

combined_plot

#monthly income and attrition

chart14 <- ggplot(df, aes(x = Attrition, y = MonthlyIncome)) +
  geom_boxplot() +
  scale_fill_grey() +
  theme(legend.position = "right")

chart14

#Median monthly icome by y

library(dplyr)

df %>%
  group_by(Attrition) %>% 
  summarise(Mean=mean(MonthlyIncome))

df <- df[,1:28]

#correlation matrix of numeric variables

library(ggplot2)
library(ggcorrplot)

nums <- select_if(df, is.numeric)
correl_matrix <- round(cor(nums),1)

chart15 <- ggcorrplot(correl_matrix, method ="square", hc.order = TRUE, type = "lower", 
           outline.color ="grey", lab = TRUE, colors = c("grey", "white", "darkgrey"))
chart15

# Naive Bayes Classification ----------------------------------------------

#Fitting Naive Bayes Model


library(e1071)
library(caTools)
library(caret)

classifier_nb <- naiveBayes(Attrition ~ ., train)
classifier_nb

#make predictions

set.seed(1)
nb_predicted <- predict(classifier_nb, test)

#Confusion Matrix

table(test$Attrition, nb_predicted)
accuracy_naive <- (236+39)/356

# Logistic regression model building --------------------------------------


lm1 <- glm(Attrition ~ . , data = train , family = binomial(link = "logit"))

options(scipen=999)
summary(lm1)

pseudo_r_1 <- with(summary(lm1), 1 - deviance/null.deviance)

lm2 <- glm(Attrition ~ . -Department, data = train , family = binomial(link = "logit"))
summary(lm2)

pseudo_r_2 <- with(summary(lm2), 1 - deviance/null.deviance)

lm3 <- glm(Attrition ~ . -Department -PercentSalaryHike, data = train , family = binomial(link = "logit"))
summary(lm3)

pseudo_r_3 <- with(summary(lm3), 1 - deviance/null.deviance)

lm4 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike, data = train , family = binomial(link = "logit"))
summary(lm4)

pseudo_r_4 <- with(summary(lm4), 1 - deviance/null.deviance)

lm5 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus, data = train , family = binomial(link = "logit"))
summary(lm5)

pseudo_r_5 <- with(summary(lm5), 1 - deviance/null.deviance)

lm6 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
           -YearsInCurrentRole, data = train , family = binomial(link = "logit"))
summary(lm6)

pseudo_r_6 <- with(summary(lm6), 1 - deviance/null.deviance)

lm7 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
           -YearsInCurrentRole - EducationField, data = train , family = binomial(link = "logit"))
summary(lm7)

pseudo_r_7 <- with(summary(lm7), 1 - deviance/null.deviance)

lm8 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
           -YearsInCurrentRole - EducationField -WorkLifeBalance, data = train , family = binomial(link = "logit"))
summary(lm8)

pseudo_r_8 <- with(summary(lm8), 1 - deviance/null.deviance)

lm9 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
           -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction, data = train , family = binomial(link = "logit"))
summary(lm9)

pseudo_r_9 <- with(summary(lm9), 1 - deviance/null.deviance)

lm10 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
            -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction
            -JobLevel, data = train , family = binomial(link = "logit"))
summary(lm10)

pseudo_r_10 <- with(summary(lm10), 1 - deviance/null.deviance)

lm11 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
            -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction
            -JobLevel - PerformanceRating, data = train , family = binomial(link = "logit"))
summary(lm11)

pseudo_r_11 <- with(summary(lm11), 1 - deviance/null.deviance)

lm12 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
            -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction
            -JobLevel - PerformanceRating -Education, data = train , family = binomial(link = "logit"))
summary(lm12)

pseudo_r_12 <- with(summary(lm12), 1 - deviance/null.deviance)

lm13 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
            -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction
            -JobLevel - PerformanceRating -Education -TotalWorkingYears, data = train , family = binomial(link = "logit"))
summary(lm13)

pseudo_r_13 <- with(summary(lm13), 1 - deviance/null.deviance)

lm14 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
            -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction
            -JobLevel - PerformanceRating -Education -TotalWorkingYears
            -YearsWithCurrManager, data = train , family = binomial(link = "logit"))
summary(lm14)

pseudo_r_14 <- with(summary(lm14), 1 - deviance/null.deviance)

lm15 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
            -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction
            -JobLevel - PerformanceRating -Education -TotalWorkingYears
            -YearsWithCurrManager -Gender, data = train , family = binomial(link = "logit"))
summary(lm15)

pseudo_r_15 <- with(summary(lm15), 1 - deviance/null.deviance)

lm16 <- glm(Attrition ~ . -JobRole -Department -PercentSalaryHike -MaritalStatus
            -YearsInCurrentRole - EducationField -WorkLifeBalance -RelationshipSatisfaction
            -JobLevel - PerformanceRating -Education -TotalWorkingYears
            -YearsWithCurrManager -Gender -TrainingTimesLastYear, data = train , family = binomial(link = "logit"))
summary(lm16)

# final model -------------------------------------------------------------

library(pixiedust)

final_model <- data.frame(dust(lm16) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 2) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-statistic", 
                    "P-value"))

write.csv(final_model, "final_model.csv")

pseudo_r_16 <- with(summary(lm16), 1 - deviance/null.deviance)

model_names <- c("lm1", "lm2", "lm3", "lm4", "lm5", "lm6", "lm7", "lm8", "lm9",
         "lm10", "lm11", "lm12", "lm13","lm14", "lm15", "lm16")

pseudo_values <- c(pseudo_r_1, pseudo_r_2, pseudo_r_3, pseudo_r_4, pseudo_r_5, pseudo_r_6,
          pseudo_r_7, pseudo_r_8, pseudo_r_9, pseudo_r_10, pseudo_r_11, pseudo_r_12,
          pseudo_r_13, pseudo_r_14, pseudo_r_15, pseudo_r_16)

pseudo_comparison <- cbind.data.frame(model_names, pseudo_values)
write.csv(pseudo_comparison, "pseudo_comparison.csv")

#variable importance

varimp <- caret::varImp(lm16)[,-2]
variables <- variable.names(lm16)[-1]

varimp <- caret::varImp(lm16)

chart16 <- varimp %>% 
 mutate(variables = fct_reorder(variables, varimp$Overall)) %>% 
 ggplot(aes(x = variables, y = varimp$Overall)) +
 labs (x = "Variable name", y = "Variable importance") +
 geom_bar(stat = "identity") + coord_flip()

chart16

#calculate VIF values for each predictor variable in our model

vif_table <- data.frame(car::vif(lm16))[,1:2]

#use the model to make predictions

set.seed(1)
predicted <- predict(lm16, test, type = "response")
predicted_table <- cbind.data.frame(predicted, test)

#model diagnostics

set.seed(1)
glm.pred=rep ("No", 356)
glm.pred[predicted >.5] = "Yes"

confusion_matrix <- table(glm.pred, test$Attrition)
confusion_matrix

accuracy <- (284+30)/356 #logistic regression model is working way better than random guessing

library(pROC)

ROC <- roc(test$Attrition, predicted) #ROC curve and AUC
AUC <- round(ROC$auc, 4)

chart17 <- ggroc(ROC, colour = 'darkgrey', size = 2) +
 ggtitle(paste0('ROC Curve ', '(AUC = ', AUC, ')'))

chart17

new_cutoff <- coords(ROC, "best", ret = "threshold", best.method = "youden")

set.seed(1)
new_glm.pred <- rep("No", 356)
new_glm.pred[predicted >new_cutoff$threshold] = "Yes"

new_confusion_matrix <- table(new_glm.pred, test$Attrition)
new_confusion_matrix

accuracy_new <- (272+39)/356
recall <- 39/(39+20)
specificity <- 272/(272+25)


# Fitting classification trees --------------------------------------------

library(tree)

train$Attrition <- as.factor(train$Attrition)

tree_attrition <- tree(Attrition~., train)
summary(tree_attrition)

plot(tree_attrition)
text(tree_attrition, pretty = 0)

tree_attrition

#use the model to make predictions

set.seed(1)
tree_predicted <- predict(tree_attrition, test, type = "class")
table(tree_predicted, test$Attrition)

accuracy_tree <- (272+22)/356 #classification tree is working way better than random guessing

tree_predicted

#pruning

set.seed(1)
cv_attrition <- cv.tree(tree_attrition, FUN = prune.misclass)
names(cv_attrition)

cv_attrition$size
cv_attrition$dev  #The trees with 4 terminal nodes results in the lowest cross-validation error rate, with 141 cross-validation errors
cv_attrition$k
cv_attrition$method

par(mfrow =c(1,2))

plot(cv_attrition$size, cv_attrition$dev, type = "b", xlab = "number of terminal nodes" , ylab = "cross-validation error rate")
plot(cv_attrition$k, cv_attrition$dev, type = "b", xlab = "cost-complexity parameter", ylab = "")

prune_tree_attrition <- prune.misclass(tree_attrition, best = 4)
plot(prune_tree_attrition)
text(prune_tree_attrition, pretty = 0)

#use pruned tree to make predictions

set.seed(1)
pruned_tree_predicted <- predict(prune_tree_attrition, test, type = "class")
table(pruned_tree_predicted, test$Attrition)

accuracy_pruned_tree <- (278+17)/356


# Bagging and Random Forest -----------------------------------------------

library(randomForest)
set.seed(1)
bag_attrition <- randomForest(Attrition ~., train, mtry = 27, importance =TRUE)
bag_attrition

#use pruned tree to make predictions

set.seed(1)
bag_predicted <- predict(bag_attrition, test)
table(bag_predicted, test$Attrition)

accuracy_bagged_tree <- (282+18)/356

#random forest at mtry = 10

set.seed(1)
rf_attrition <- randomForest(Attrition~., train, mtry = 10, importance =TRUE)
rf_predicted <- predict(rf_attrition, test)

table(rf_predicted, test$Attrition)

accuracy_rf <- (287+16)/356

#variable importance

importance(rf_attrition)

varImpPlot(rf_attrition, bg = "darkgrey", cex=1)


# Comparison of models ----------------------------------------------------

model_names <- c( "Naive Bayes", "LR", "Classification Tree", "Bootstrap", "Random Forest")

#Accuracy

accuracy_comp <- round(c(accuracy_naive, accuracy, accuracy_pruned_tree, accuracy_bagged_tree, accuracy_rf),4)

#Recall

table(nb_predicted, test$Attrition)
recall_nb <- 39/(39+20)

table(pruned_tree_predicted, test$Attrition)
recall_tree <- 17/(17+42)

table(bag_predicted, test$Attrition)
recall_bootstrap <- 16/(16+43)

table(rf_predicted, test$Attrition)
recall_rf <- 16/(16+43)

recall_comp <- round(c(recall_nb, recall, recall_tree, recall_bootstrap, recall_rf),4)

summary(tree_attrition)
summary(lm16)

#Cohen's kappa

library(psych)

cohen <- cohen.kappa(cbind(nb_predicted, glm.pred, pruned_tree_predicted, bag_predicted, rf_predicted))
cohen_comp <- cohen$av.wt #fair agreement

model_comparison <- cbind(model_names, accuracy_comp, recall_comp)
write.csv(model_comparison, "model_comparison.csv")

write.csv(confusion_matrix, "confusion_matrix_glm.csv")
