---

title: "Statistics Project"
author: "Prem_Shah"
date: "11/6/2019"
output: html_document

---




```{r}

# lets load the data from the package and get started

#install.packages("nycflights13",repos='http://cran.us.r-project.org')
library(nycflights13)
Flights <- flights
weather <- weather
airlines  <- airlines

```


```{r}
################ 1- Descriptive statistics (10 points) - show at least two things here #############################
# 1.a Describe the data types (numeric vs. categorical) and distributions (you can do this visually or with a package)
str(Flights)
str(airlines)
str(weather)

# 1.b Describe the target variable. What are the units?

#  Lets see the correlation between departure delay and arrival delay we are excluding NA's i.e. cancelled flights to make sense in our correlation
cor(Flights$dep_delay,Flights$arr_delay,use = "pairwise.complete.obs") 

# we can see that there is strong positive correlation between departure delay and arrival delay. The value is 0.91. Since departure delay is the cause of arrival delay we will just focus on the departure delay. 

print("Since our motive is to predict whether a flight will be delayed or cancelled or will be on time, we have categorize our target variable with (1) and (0). If the flight is delayed for more than 15 minutes (as per the business rule) or it is cancelled we have categorize it as (1) and (0) otherwise. In order to able to group the flight cancellation and the delay into (1) we need to use tidyverse and dplyr to play with our dataset.")

# 1.c Show univariate plots like boxplots, histograms
print("For exploring the distributions lets do the some pre-processing to our flights dataset and converting our continous target variable to categorical target variable")

library(tidyverse)
library(dplyr)
library(lubridate)
pre_flight <- Flights %>%
mutate(delay = ifelse(dep_delay >= 15 | is.na(dep_delay) == TRUE, 1, 0), # mutate helps us to make changes within the dataframe
week_day = wday(time_hour, label=TRUE, abbr = FALSE),
week_month = month(time_hour, label=TRUE, abbr = FALSE),
carrier = factor(carrier),
origin = factor(origin)) %>%
#select relevant variables and save to a new data table
select(delay, year, month, week_month, day, week_day, carrier, origin, distance, hour, time_hour)
head(pre_flight)
summary(pre_flight)

# lets see the distribution of the delayed flights and cancelled flights in a month with the density line
pre_flight %>% filter(pre_flight$delay == 1) %>% group_by(month) %>% summarize(count_delays = n()) %>%
ggplot(aes(x= month, y = count_delays)) +
geom_point() +
geom_line(col="Red") +
scale_x_discrete(limits=1:12) + # since we have 12 months there cant be a continous number
ylab("Number of Flights")

# lets see the distribution of the delayed flights and cancelled flights in a month using histogram 
pre_flight %>% filter(pre_flight$delay == 1) %>% group_by(month) %>% summarize(count_delays = n()) %>%
ggplot(aes(x= month, y = count_delays)) +
geom_col(col="red") +
scale_x_discrete(limits=1:12) + # since we have 12 months there cant be a continous number
ylab("Number of Flights")

# lets plot a boxplot between our response variable and the predictor variable. Since our predictor variables are majorly coming from the weatherdataset and we are going to use airlines dataset for the analysis purpose lets merge all the dataset 

# now lets join the cleaned pre_flight dataset to our airlines
pre_flight_airline <- left_join(pre_flight, airlines, by="carrier")
View(pre_flight_airline)

# now lets join pre_flights_airlines data with weather for model building
modeldata <- left_join(x = pre_flight_airline, y = weather, by = c("origin","time_hour", "year", "month", "day", "hour"))
modeldata
summary(modeldata)

#lets explore the missing values that came after we attached weather dataset 
colSums(is.na(modeldata))

# we can see that data for the wind_gust is missing for more than 50% of the data. we can also delete the irrelavant columns that are not being used in our analysis.

# lets delete the variables "time_hour","wind_gust","Year","name of the airlines")
modeldata$wind_gust <- NULL
modeldata$time_hour <- NULL
modeldata$year <- NULL
modeldata$name <- NULL
modeldata$week_month <- NULL

colSums(is.na(modeldata))

# removing NA values from all the rows 
modeldata1 <- na.omit(modeldata)

# lets plot the box plot between temp and delay
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=temp,col=delay)) +
  geom_boxplot()

# lets plot the box plot between dewpp and delay
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=dewp,col=delay)) +
  geom_boxplot()

# lets plot the box plot between humid and delay 
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=humid,col=delay)) +
  geom_boxplot()

# lets plot the box plot between wind direction and delay 
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=wind_dir,col=delay)) +
  geom_boxplot()

# lets plot the box plot between wind speed and delay 
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=wind_speed,col=delay)) +
  geom_boxplot()


# lets plot the box plot between precipitation and delay 
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=precip,col=delay)) +
  geom_boxplot()

# lets plot the box plot between pressure and delay 
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=pressure,col=delay)) +
  geom_boxplot()

# lets plot the box plot between visibility and delay 
modeldata1%>%mutate(delay=factor(delay))%>%
  ggplot(aes(x=delay,y=visib,col=delay)) +
  geom_boxplot()


```


```{r}
################### 2. Exploratory data analysis (10 points) - show at least two things here #########################################
# 2.a Correlations/pairwaise correlation (Pearson vs. Spearman)
# pearson correlation 
#install.packages("ggpubr",repos='http://cran.us.r-project.org')
library(ggpubr)
ggscatter(Flights, x = "dep_delay", y = "arr_delay",
add = "reg.line", conf.int = TRUE,
cor.coef = TRUE, cor.method = "pearson", use = "pairwise.complete.obs",
xlab = "Departure Delay", ylab = "Arrival Delay")

# also do it for spearman 
ggscatter(Flights, x = "dep_delay", y = "arr_delay",
add = "reg.line", conf.int = TRUE,
cor.coef = TRUE, cor.method = "spearman", use = "pairwise.complete.obs",
xlab = "Departure Delay", ylab = "Arrival Delay")

ggscatter(modeldata1, x = "temp", y = "dewp",
add = "reg.line", conf.int = TRUE,
cor.coef = TRUE, cor.method = "pearson", use = "pairwise.complete.obs",
xlab = "temperature", ylab = "dew point")

library(psych)
# pearson correlation _ we are adding this to the comments becausew it is taking too much time to convert
#pairs.panels(modeldata[,c(1,8,9,10,11,12,13,14,15)], 
#             method = "pearson", # correlation method
#            hist.col = "#00AFBB",
#             density = TRUE,  # show density plots
#             ellipses = TRUE # show correlation ellipses
#)

```


```{r}
############################## 3. Probability concepts (10 points) ###################################
# If you have two categories, great! Make a table like this and calculate joint and marginal probabilities.

Airport_table <- xtabs(~delay + origin, data = pre_flight_airline)
Airport_table

addmargins(Airport_table)

addmargins(prop.table(Airport_table))

# lets calculate joint marginal probabilities of flight delays with respect to airport 

JFK_delays <- 25210/81169 
JFK_delays

EWR_delays <- 33014/81169
EWR_delays

LGA_delays <- 22945/81169
LGA_delays

# contingency table for airlines and delays/no delays

Flights_table <- xtabs(~delay + name, data = pre_flight_airline)
Flights_table

addmargins(Flights_table)


# joint and marginal probabilities
addmargins(prop.table(Flights_table))

# contingency table for weekdays and delays/no delays
day_table <- xtabs(~delay + week_day, data = modeldata)
day_table

addmargins(day_table)

# joint and marginal probabilities 
addmargins(prop.table(day_table))


# contingency table for month and delays/no delays
pre_flight_table <- xtabs(~delay + week_month, data = pre_flight)
pre_flight_table

addmargins(pre_flight_table)


# joint and marginal probablities for month delays 
January_delays <- 5612/81169
January_delays

July_delays <- 9579/81169
July_delays


```


```{r}
######################### 4. Chi-square test (10 points) ####################

chisq.test(pre_flight_airline$delay, pre_flight_airline$week_month, correct = F)
print("Since the p-value of chi-squred test is less than 0.05, we reject the null hypothesis and conclude that the variables week_month and delays are related")

```


```{r}
##################### 5. Data prep ##################################
print("we have already cleaned the data in the above steps. we had many missing values in the wind_gust column. we had about 4% of the missing data which is very less. we deleted the irrelevant data for the model building ")

# lets create correlation matrix and find out the correlation between our target variables and predictor variables 
#install.packages("caret",repos='http://cran.us.r-project.org')
library(tidyverse)
correlationmatrix <- modeldata1 %>% 
  select_if(is.numeric) %>%
  cor(.)
correlationmatrix


# since we are predicting the categorical variable lets see the odds ratio of "1" in our dataset. The more the odds of "1" the greater will be the prediction of 1 

#Number of flight delays and cancellations in the dataset
table(modeldata1$delay)


#Proportion of flight delays and cancellations in the dataset
round(table(modeldata1$delay)/nrow(modeldata1),2)

print("We can see that only 22% of our dataset has 1 which means we need to do the oversampling for the model")
 
# before doing the oversampling lets first partion our dataset
# splitting the dataset into "training"(60%) and "Validation" (40%)

split <- sample(2, nrow(modeldata1), replace=TRUE,prob = c(0.6,0.4))
trainingdata <- modeldata1[split==1,]
trainingdata$month <- NULL
trainingdata$day <- NULL
trainingdata$week_day <- NULL
trainingdata$carrier <- NULL
trainingdata$origin <- NULL
trainingdata$distance <- NULL
trainingdata$hour <- NULL

# lets create a validation partition
validationdata <- modeldata1[split==2,]
validationdata$month <- NULL
validationdata$day <- NULL
validationdata$week_day <- NULL
validationdata$carrier <- NULL
validationdata$origin <- NULL
validationdata$distance <- NULL
validationdata$hour <- NULL

# lets do the oversampling for training data 

#install.packages("ROSE",repos='http://cran.us.r-project.org')
library(ROSE)

table(trainingdata$delay)

#over sampling 
over_training <- ovun.sample(delay ~ ., data = trainingdata, method = "both",p = 0.5,seed = 1)$data
# keeping only the weather data
over_training$month <- NULL
over_training$day <- NULL
over_training$week_day <- NULL
over_training$carrier <- NULL
over_training$origin <- NULL
over_training$distance <- NULL
over_training$hour <- NULL
table(over_training$delay) # now we can see that we have balance our target variable between 1 and 0


# lets do the oversampling for the validation data
library(ROSE)

table(validationdata$delay)
#over sampling 
over_validation <- ovun.sample(delay ~ ., data = validationdata, method = "both",p = 0.5,seed = 1)$data

# keeping only the weather dataset
over_validation$month <- NULL
over_validation$day <- NULL
over_validation$week_day <- NULL
over_validation$carrier <- NULL
over_validation$origin <- NULL
over_validation$distance <- NULL
over_validation$hour <- NULL
table(over_validation$delay) # now we can see that we have balance our target variable between 1 and 0

    

```


```{r}
################################### 6. Decide on which regression you will use #########################
print("We have converted our target variable into the categorical variable. Since our motive is to predict whether a flight will be delayed or cancelled or will be on time, we have categorize our target variable with (1) and (0). If the flight is delayed for more than 15 minutes (as per the business rule) or it is cancelled we have categorize it as (1) and (0) otherwise.")
```


```{r}
################################# 7. Proficiency in stepwise regression (40 points) ###############################

########### Model building ##############

# lets first run the full model without oversampling of the data 
# 7.a lets run the GLM model with all the variables (full model) - using training data 
Model1 <- glm(delay~.,family = binomial, data = trainingdata) 
summary(Model1)


# lets create a full model with oversampled (Balanced) dataset for stepwise regression 
full.over_train <- glm(delay~.,family = binomial, data = over_training) 
summary(full.over_train)


# lets check the collineratiy 
library(car)

vif(full.over_train)

# we can see that the highest VIF is for temp and dewp. we can delete the dewp since it has the highest VIF 

# lets create model with deleted dewp - lets train the model using overtraining dataset ( Reduced Model)
over.reduced_train <- glm(delay~.-dewp, family = binomial,data = over_training)

summary(over.reduced_train)

library(car)
vif(over.reduced_train )

# all VIF are below 5


###### 7.b Fit a null model ##################
null.fit <- glm(delay~1, data=over_training, family = binomial)
summary(null.fit)



############## 7.c Using some form of stepwise regression (forward, backward, both), fit a reduced model.#########################
# lets try the stepwise regression to our tempmodel using only the validation data


# lets try a forward variable selection # adding this to the comment because taking a lot time to convert it to HTML
#forwards_model <- step(null.fit,
#                scope=list(lower=formula(null.fit),
#                           upper=formula(full.over_train)), 
#                direction="forward")
#summary(forwards_model)

# lets try a backward variable selection
backwards_model <- step(full.over_train)
summary(backwards_model)



# lets do both starting from the null model
both.null_model <- step(null.fit,
                scope=list(lower=formula(null.fit),
                           upper=formula(full.over_train)), 
                direction="both")
summary(both.null_model)


###################### 7.d. Compare the model output and note any trends that you see. ###################################


```


```{r}
############### 8. Model interpretation (30 points) ###################################
# 8.a For each model you fit, interpret at least two coefficients. What trends do you see in the model output (parameter estimates, p-values, etc)?
#################################

# First, all variables are significant.

# Now let's see the interpretations for some variables

# Humidity Relative humidity 2.260e-02  
# The coefficient of humidity (Humidity) is positive, meaning that when humidity increases, the probability of delay also increases. First, the higher the humidity makes the lower the pressure of air per unit. So, the airplane will not get enough lift for taking off. Second, high humidity will influence engines of airplanes. Airplanes’ engines are designed for cold and dry air. High humidity will make the oxygen molecules fewer in the air, which is used to burn the engines. Then engines will not have enough oxygen molecules to burn and cannot get enough thrust for launch. So, high humidity will increase the probability of delay.

# Precip Precipitation, in inches 5.115e+00  
# The coefficient of precipitation (Precip) is positive, meaning that when precipitation increases, the probability of delay also increases. Because when precipitation goes up, it means that the weather is rain, snow or other likewise situation. It will make airstrip condition worse than usual. And if the reason of the extreme weather is the thundercloud, it will be dangerous for airplane. So, as precipitation (Precip) goes up, the probability of delay will increase.

# Pressure Sea level pressure in millibars -2.941e-02  
# The coefficient of pressure (Pressure) is negative, meaning that when pressure increases, the probability of delay will decrease as well. From Bernoulli’s equation, we know that the lift force on airplane, which helps airplane to overcome the gravity, is influenced by pressure. When pressure is low, the relative dynamic pressure around the airplane when it is running on the airstrip will also be low. Then the lift of airplane will not be enough. So, the airplane cannot take off.

# Visib Visibility in miles -2.328e-02  
# The coefficient of visibility (Visib) is negative, meaning that when visibility increases, the probability of delay will decrease as well. Though our technology is relatively high, the launch of the airplane still needs pilots to control. If visibility is low, it will make pilots mistake airstrips and direction. So, when visibility is under a level, the airplane can not take off, causing delay.


```


```{r cars}
################################ 9. Model fit diagnostics for each model you fit (30 points) #################################
### Confusion matrix for binary (TPR, TNR, FPR, FNR)

# prediction result for full model (without oversampled)
# full model with training data 
Model1pred.train <- predict(Model1, newdata = trainingdata, type = "response")
head(Model1pred.train)

# convert to a class of predicted 0s and 1s
round.Model1pred.train <- as.numeric(ifelse(Model1pred.train >=0.5, 1, 0))
table(round.Model1pred.train, trainingdata$delay)

# install SDMTools
#install.packages("SDMTools",repos='http://cran.us.r-project.org')
library(SDMTools)
Model1pred.train.results <- accuracy(obs = trainingdata$delay,
                       pred=round.Model1pred.train)
Model1pred.train.results

# full model with validation data 
Model1pred_val <- predict(Model1, newdata = validationdata, type = "response")
head(Model1pred_val)

# convert to a class of predicted 0s and 1s
round.Model1pred_val <- as.numeric(ifelse(Model1pred_val >=0.5, 1, 0))
table(round.Model1pred_val, validationdata$delay)

library(SDMTools)
Model1pred_val.results <- accuracy(obs = validationdata$delay,
                       pred=round.Model1pred_val)
Model1pred_val.results

# full model with the oversampled data we created a model using training data we are going to evaluate validation  balanced dataset as we will be comparing only validation dataset (Full Model with oversampled)

# prediction result for full model using balanced training dataset
Bal_train.pred <- predict(full.over_train, newdata = over_training, type = "response")
head(Bal_train.pred)

# convert to a class of predicted 0s and 1s
round.Bal_train.pred <- as.numeric(ifelse(Bal_train.pred >=0.5, 1, 0))
table(round.Bal_train.pred, over_training$delay)

library(SDMTools)
Bal_train.results <- accuracy(obs = over_training$delay,
                       pred=round.Bal_train.pred)
Bal_train.results

# prediction result for full model using balanced validation dataset 
Bal_val.pred <- predict(full.over_train, newdata = over_validation, type = "response")
head(Bal_val.pred)

# convert to a class of predicted 0s and 1s
round.Bal_val.pred <- as.numeric(ifelse(Bal_val.pred >=0.5, 1, 0))
table(round.Bal_val.pred, over_validation$delay)

library(SDMTools)
Bal_val.results <- accuracy(obs = over_validation$delay,
                       pred=round.Bal_val.pred)
Bal_val.results


# for the reduced model (oversampled)
# prediction result after removing variables as per the VIF analysis. WHich is the reduced model 
VIF.pred_train <- predict(over.reduced_train, newdata = over_training, type = "response")
head(VIF.pred_train)

# convert to a class of predicted 0s and 1s
round.VIF.pred_train  <- as.numeric(ifelse(VIF.pred_train >=0.5, 1, 0))
table(round.VIF.pred_train, over_training$delay)

library(SDMTools)
VIF.train.results <- accuracy(obs = over_training$delay,
                       pred=round.VIF.pred_train )
VIF.train.results

# for validation data
VIF.pred_val <- predict(over.reduced_train, newdata = over_validation, type = "response")
head(VIF.pred_val)

# convert to a class of predicted 0s and 1s
round.VIF.pred_val <- as.numeric(ifelse(VIF.pred_val >=0.5, 1, 0))
table(round.VIF.pred_val, over_validation$delay)

library(SDMTools)
VIF.val.results <- accuracy(obs = over_validation$delay,
                       pred=round.VIF.pred_val)
VIF.val.results


#prediction result for null model 
# for training model 
pred.null_train <- predict(null.fit, newdata = over_training, type = "response")
head(pred.null_train )

round.pred.null_train <- as.numeric(ifelse(pred.null_train  >=0.5, 1, 0))

table(round.pred.null_train, over_training$delay)

library(SDMTools)
pred.null_train.results <- accuracy(obs = over_training$delay,
                       pred=round.pred.null_train )
pred.null_train.results

# for validation model 
pred.null_val <- predict(null.fit, newdata = over_validation, type = "response")
head(pred.null_val)

round.pred.null.val <- as.numeric(ifelse(pred.null_val  >=0.5, 1, 0))

table(round.pred.null.val, over_validation$delay)

library(SDMTools)
pred.null_val.results <- accuracy(obs = over_validation$delay,
                       pred=round.pred.null.val)
pred.null_val.results

    
#prediction result for stepwise regression forward model 
# for training data 
#pred.forwards.train <- predict(forwards_model, newdata = over_training, type = "response")
#head(pred.forwards.train)

#pred.forwards.train.class<- as.numeric(ifelse(pred.forwards.train >=0.5, 1, 0))

#table(pred.forwards.train.class, over_training$delay)

#library(SDMTools)
#forwards.train.results <- accuracy(obs = over_training$delay,
#                       pred=pred.forwards.train.class)
#forwards.train.results

# for validation data
#pred.forwards.val <- predict(forwards_model, newdata = over_validation, type = "response")
#head(pred.forwards.val)

#pred.forwards.val.class<- as.numeric(ifelse(pred.forwards.val >=0.5, 1, 0))

#table(pred.forwards.val.class, over_validation$delay)

#library(SDMTools)
#forwards.val.results <- accuracy(obs = over_validation$delay,
#                       pred=pred.forwards.val.class)
#forwards.val.results

    
#prediction result for stepwise regression backward
# for training data 
pred.backwards.train <- predict(backwards_model, newdata = over_training, type = "response")
head(pred.backwards.train)

pred.backwards.train.class <- as.numeric(ifelse(pred.backwards.train >=0.5, 1, 0))

table(pred.backwards.train.class, over_training$delay)

library(SDMTools)
backwards.train.results <- accuracy(obs = over_training$delay,
                       pred=pred.backwards.train.class)
backwards.train.results

# for validation 
pred.backwards.val <- predict(backwards_model, newdata = over_validation, type = "response")
head(pred.backwards.val)

pred.backwards.val.class <- as.numeric(ifelse(pred.backwards.val >=0.5, 1, 0))

table(pred.backwards.val.class, over_validation$delay)

library(SDMTools)
backwards.val.results <- accuracy(obs = over_validation$delay,
                       pred=pred.backwards.val.class)
backwards.val.results


#prediction result for stepwise regression bothways from null
# for training data 
pred.both.train <- predict(both.null_model, newdata = over_training, type = "response")
head(pred.both.train)

pred.both.train.class <- as.numeric(ifelse(pred.both.train >=0.5, 1, 0))


table(pred.both.train.class, over_training$delay)

library(SDMTools)
pred.both.train.results <- accuracy(obs = over_training$delay,
                       pred=pred.both.train.class)
pred.both.train.results

# for validation data 
pred.both.val <- predict(both.null_model, newdata = over_validation, type = "response")
head(pred.both.val)

pred.both.val.class <- as.numeric(ifelse(pred.both.val >=0.5, 1, 0))


table(pred.both.val.class, over_validation$delay)

library(SDMTools)
pred.both.val.results <- accuracy(obs = over_validation$delay,
                       pred=pred.both.val.class)
pred.both.val.results


```


```{r}
################# 10. Model comparison for full model and reduced model (10 points) ##################################
AIC(full.over_train)
AIC(over.reduced_train)
BIC(full.over_train)
BIC(over.reduced_train)

# Do you reach the same conclusion using both of these methods?
print("We can see that using the AIC and BIC function to compare the model we can see that we got the same results i.e AIC for full.over_train and BIC for Full.over_train is better than the over.reduced_train")
```

