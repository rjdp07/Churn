library(tidyverse)
library(data.table)
library(caret)
library(broom)
library(Boruta)
library(mlbench)
library(rpart)
library(rpart.plot)


churn_data = fread("churn_data.csv") %>% as_tibble()

#Check customer ID Cardinality

nrow(churn_data) == length(churn_data$customerID %>% unique())

#Check gender values
churn_data %>% 
  count(gender, sort = TRUE)

#Check Senior Citizen Values
churn_data %>% 
  count(SeniorCitizen)
#Majority of our subscribers are still not senior Citizen

#Check Partner column values
churn_data %>% 
  count(Partner, sort = TRUE)
#We have more single subscribers

#Check Dependents column values
churn_data %>% 
  count(Dependents, sort = TRUE)
#We have more subscribers that does not have any dependent

#Dependent and Partner visual

churn_data %>% 
  ggplot(aes(x = Partner, fill = Dependents)) +
  geom_bar(position = "dodge")


#Check tenure

churn_data %>% 
  ggplot(aes(x = tenure)) +
  geom_histogram()
#Not normal distribution

#Check count for Yes or No Columns
churn_data %>% 
  count(PhoneService, sort = TRUE)

churn_data %>% 
  count(MultipleLines, sort = TRUE)

churn_data %>% 
  count(InternetService, sort = TRUE)

churn_data %>% 
  count(OnlineSecurity, sort = TRUE)

churn_data %>% 
  count(OnlineBackup, sort = TRUE)

churn_data %>% 
  count(DeviceProtection, sort = TRUE)

churn_data %>% 
  count(TechSupport, sort = TRUE)

churn_data %>% 
  count(StreamingTV, sort = TRUE)

churn_data %>% 
  count(StreamingMovies, sort = TRUE)

churn_data %>% 
  count(Contract, sort = TRUE)

churn_data %>% 
  count(PaperlessBilling, sort = TRUE)


churn_data %>% 
  count(PaymentMethod, sort = TRUE)

churn_data %>% 
  ggplot(aes(x = MonthlyCharges)) +
  geom_histogram()

churn_data %>% 
  ggplot(aes(x = TotalCharges)) +
  geom_histogram()

churn_data %>% 
  count(Churn, sort = TRUE)

#Now Lets relate these predictor to our respondent variable

churn_data %>% 
  ggplot(aes(x = Churn,fill = gender)) +
  geom_bar(position = "dodge")
#No relation for gender
churn_data %>% 
  ggplot(aes(x = Churn,fill = as.factor(SeniorCitizen))) +
  geom_bar(position = "dodge") +
  labs(fill = "SeniorCitizen")

#No relation for SeniorCitizen
churn_data %>% 
  ggplot(aes(x = Churn, fill = Partner)) +
  geom_bar(position = "dodge")
#There are more taken subscribers that did not churn compare to those subscriber that did not churn but is single
#There are more single subscriber that did churn compare to those subscribers that did churn but it taken

churn_data %>% 
  ggplot(aes(x = Churn, fill = Dependents)) +
  geom_bar(position = "dodge")
#Same behavior or count for Dependents on both Yes and No for Churn

churn_data %>% 
  ggplot(aes(x = tenure, fill = Churn)) +
  geom_histogram()


churn_data %>% 
  ggplot(aes(x = Churn, fill = PhoneService)) +
  geom_bar(position = "dodge")

churn_data %>% 
  ggplot(aes(x = Churn, fill = MultipleLines)) +
  geom_bar(position = "dodge")
#Little to No Pattern display

churn_data %>% 
  ggplot(aes(x = Churn, fill = InternetService)) +
  geom_bar(position = "dodge")
#Majority of the customer that churned has a fiber optic Internet Service -- Issues with Fiber optic??

churn_data %>% 
  ggplot(aes(x = Churn, fill = OnlineSecurity)) +
  geom_bar(position = "dodge")
#Those who didn't have any online security are more like to churn compare

churn_data %>% 
  ggplot(aes(x = Churn, fill = OnlineBackup)) +
  geom_bar(position = "dodge")
#Those who didn't have any online backup are more like to churn compare

churn_data %>% 
  ggplot(aes(x = Churn, fill = DeviceProtection)) +
  geom_bar(position = "dodge")
#Those who didn't have any DeviceProtection are more like to churn compare

churn_data %>% 
  ggplot(aes(x = Churn, fill = TechSupport)) +
  geom_bar(position = "dodge")
#Those who didn't have any TechSupport are more like to churn compare

churn_data %>% 
  ggplot(aes(x = Churn, fill = StreamingTV)) +
  geom_bar(position = "dodge")
#Those who didn't have any StreamingTV are more like to churn compare

churn_data %>% 
  ggplot(aes(x = Churn, fill = StreamingMovies)) +
  geom_bar(position = "dodge")

#Those who didn't have any StreamingMovies are more like to churn compare


churn_data %>% 
  ggplot(aes(x = Churn, fill = Contract)) +
  geom_bar(position = "dodge")
#Month-to-Mont contract subscriber tend to have more churn count


churn_data %>% 
  ggplot(aes(x = Churn, fill = PaperlessBilling)) +
  geom_bar(position = "dodge")

#Customer with Paperless Billing have higher count of churn entries

churn_data %>% 
  ggplot(aes(x = Churn, fill = PaymentMethod)) +
  geom_bar(position = "dodge")
#Customers who used Electronic Check has a high count of churn

churn_data %>% 
  ggplot(aes(x = MonthlyCharges, fill = Churn)) +
  geom_histogram()

churn_data %>% 
  ggplot(aes(x = TotalCharges, fill = Churn)) +
  geom_histogram()


churn_data = churn_data %>% 
  mutate(
    Total_Month_Diff_Charge = MonthlyCharges - TotalCharges
  )

churn_data %>% 
  ggplot(aes(x = Total_Month_Diff_Charge, fill = Churn)) +
  geom_histogram()


#Check imbalance in data

xtabs(~ Churn + gender, data = churn_data)
xtabs(~ Churn + SeniorCitizen, data = churn_data)
xtabs(~ Churn + Partner, data = churn_data)


#Data Prep
churn_data = churn_data %>% 
  mutate(
    Churn = as.factor(Churn)
  )

#Remove Id column
churn_data_model = churn_data %>% 
  select(-customerID) %>% 
  mutate(
    Churn = ifelse(Churn == "Yes",1,0)
  ) %>% 
  filter(!is.na(TotalCharges))

#Split Data into Test and Train using the Straight Forward Approach


set.seed(123)
training.samples <- churn_data_model$Churn %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- churn_data_model[training.samples, ]

train.data = train.data %>% 
  mutate_if(is.character,as.factor)

test.data <- churn_data_model[-training.samples, ]


test.data = test.data %>% 
  mutate_if(is.character,as.factor)


#Simple Logistic Regression
logistic_model = glm(Churn ~ ., data = train.data, family = "binomial")

logit_summary = summary(logistic_model)

logit_summary_tb = tidy(logistic_model)

#Filter all signficant estimates
logit_summary_tb %>% 
  filter(p.value < 0.05)


#Evaluate Model
probabilities <- logistic_model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)


confusionMatrix(test.data$Churn %>% as.factor(),predicted.classes %>% as.factor())
#Without doing any feature engineering, we got an accuracy of 81.29%

#Lets try applying Feature selection

set.seed(111)


boruta = Boruta(Churn ~ ., data = churn_data_model, doTrace = 2)
plot(boruta, las = 2, cex.axis = 0.5)


#Tentative Attributes Fix
bor = TentativeRoughFix(boruta)

#Let's use the confirmed attributes as part of our model

getNonRejectedFormula(bor)

featured_logit = glm(getNonRejectedFormula(bor), data = train.data,family = "binomial" )


#Evaluate Model
probabilities_ft <- featured_logit %>% predict(test.data, type = "response")
predicted.classes_ft <- ifelse(probabilities_ft > 0.5, 1, 0)


confusionMatrix(test.data$Churn %>% as.factor(),predicted.classes_ft %>% as.factor())

#Not much of a help but at least we now know how to automate feature selection

#Lets try to use decision tree
#For the first version, we can check what will be the performance of the model without doing any
#feature selection
tree_model_all <- rpart(Churn ~ ., data = train.data,method = "class")

rpart.plot(tree_model_all)
#As per rpart.plot, looks like our model only needs 5 features to predict

#Check on train data
p_train <- predict(tree_model_all, train.data, type = 'class')
confusionMatrix(p_train %>% as.factor(), train.data$Churn %>% as.factor())

#Check on Test Data
p_test <- predict(tree_model_all, test.data, type = 'class')
confusionMatrix(p_test %>% as.factor(), test.data$Churn %>% as.factor())
#Our Decision tree model was able to achieve 80% over all accuracy just by using 5 features
#That is not bad compared to our logistic regression model that's using 18 features to have an accuracy of 81%

#Lets try to use the boruta selected features on decision tree and see if it will help
tree_boruta = rpart(getConfirmedFormula(bor), data = train.data, method = "class")

p_test_boruta <- predict(tree_boruta, test.data, type = 'class')
confusionMatrix(p_test_boruta %>% as.factor(), test.data$Churn %>% as.factor())

#Literally no change in the accuracy. What if we try to use the selected feautre by our decision tree on our
#logistic model?

logit_tree_model = glm(Churn ~ Contract + InternetService + tenure + TechSupport, data = train.data, family = "binomial")


probabilities_logit_tree <- logit_tree_model %>% predict(test.data, type = "response")
predicted.classes_logit_tree <- ifelse(probabilities_logit_tree > 0.5, 1, 0)


confusionMatrix(test.data$Churn %>% as.factor(),predicted.classes_logit_tree %>% as.factor())

#Good enough accuracy. So as a conclusion, we can try to use decision tree as well to identify
#Highly impacting attributes for our model, for much larger data sets, we can use boruta algorithm


