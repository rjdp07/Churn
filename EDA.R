library(tidyverse)
library(data.table)

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


