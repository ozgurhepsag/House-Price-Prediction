install.packages("pacman")
library(pacman)
pacman:: p_load(corrplot, ggplot2, dplyr, caret, lubridate, ggthemes, RColorBrewer, tidyverse, Metrics)

options(scipen=999)

file_path <- "C:/Users/ABRA/Desktop/CME4403 Introduction to Machine Learning/Project/kc_house_data.csv"
kc_house <- read.csv(file = file_path, header = TRUE, sep = ",", dec = ".")

# ==== Exploratory Data Analysis and Visualization ====

# Visualize the data
head(kc_house)
str(kc_house)
dim(kc_house)
summary(kc_house)

# Check for NULL values
apply(kc_house, 2, function(x) sum(is.na(x))) 

hist(kc_house$bedrooms)
boxplot(kc_house$bedrooms)
hist(kc_house$grade)

max(kc_house$bedrooms)

hist(kc_house$bathrooms)
boxplot(kc_house$bathrooms)

hist(kc_house$price)

plot(main = "", density(kc_house$price), col = "blue", xlab = "Distribution of the house price")

hist(log(kc_house$price))

# Percentage of the waterfron = 0
per_waterfront0 <- nrow(kc_house[kc_house$waterfront == 0,]) / nrow(kc_house[]) * 100 

# Percentage of the waterfron = 0
per_view0 <- nrow(kc_house[kc_house$view == 0,]) / nrow(kc_house[]) * 100 

ggplot(kc_house, aes(x=yr_renovated)) + geom_histogram() + 
  ggtitle("Number of Renovated Years") + theme(plot.title = element_text(hjust = 0.5))
  
# Percentage of the yr_renovated = 0
per_yr_renovated <- nrow(kc_house[kc_house$yr_renovated == 0,]) / nrow(kc_house) * 100
cat("Percentage of the yr_renovated = 0 is", per_yr_renovated)

ggplot(kc_house, aes(x=view)) + geom_histogram(fill="blue") + 
  ggtitle("Histogram for View") + theme(plot.title = element_text(hjust = 0.5))

# Percentage of the waterfront = 0
per_waterfront <- nrow(kc_house[kc_house$waterfront == 0,]) / nrow(kc_house) * 100
cat("Percentage of the waterfron = 0 is", per_waterfront)

ggplot(kc_house, aes(waterfront,fill=waterfront)) + geom_histogram(stat="count", fill=c("blue", "green4")) + theme_economist() +
  ggtitle("The Broker Had More Luck in 2014") + theme(plot.title = element_text(hjust = 0.5))
  
ggplot(kc_house,aes(x=price))+geom_density(fill="tomato4")

ggplot(kc_house,aes(x=log(price)))+geom_density(fill="tomato4")

ggplot(kc_house,aes(x=sqft_living))+geom_histogram(binwidth=50,fill="tomato")

ggplot(kc_house,aes(x=bathrooms)) + geom_histogram(fill="green4",binwidth=0.5,size=0.1) +
  scale_x_continuous(limits=c(1,8))

ggplot(kc_house, aes(kc_house$zipcode)) + stat_bin(binwidth=1, colour="black", fill="green")

mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))

kc_house %>% filter(bedrooms<30)%>%
  ggplot(aes(x=bedrooms,y=price,col=bedrooms))+
  geom_point(alpha=0.5,size=2)+
  geom_smooth(method="lm",se=F)+
  labs("title=Bedrooms vs Price")+scale_color_gradientn(colors=mycolors)+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=bathrooms,y=price,col=bathrooms))+
  geom_point(alpha=0.5,size=2)+
  geom_smooth(method="lm",se=F)+
  labs("title=Bedrooms vs Price")+scale_color_gradientn(colors=mycolors)+theme(legend.position="none")

ggplot(kc_house, aes(yr_built))+geom_histogram(binwidth=5,fill="tomato",alpha=0.5)+
  scale_x_continuous(limits=c(1900,2016))

ggplot(kc_house, aes(yr_built, price)) +
  geom_smooth(se = FALSE, colour = "dodgerblue3") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() +
  theme(text = element_text(face = "bold"))

ggplot(data = kc_house, mapping = aes(x = sqft_living, y = price)) + 
  geom_point(colour = 'red') + geom_smooth(method = 'lm')

corr = cor(kc_house[,3:21], method = "pearson")
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4"
         , addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7
         , cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

# ==== Data Preparation ====

# Handle outliers with Clamp Transformation on bedrooms and bathrooms features
nrow(kc_house[kc_house$bathrooms>4 | kc_house$bathrooms<1,])

# 33 bedrooms with 1.75 bathrooms in a sqft_living of 1620 with a price of 640000 makes no sense. 
kc_house[kc_house$bedrooms == 33, ] 

# 11 bedrooms with 3 bathrooms in a sqft_living of 3000 with a price of 520000 makes no sense.
kc_house[kc_house$bedrooms == 9, ] 

# Remove them
dim(kc_house <- kc_house[-c(8758,15871), ])

tapply(kc_house$price,kc_house$bedrooms,length)
tapply(kc_house$price,kc_house$bedrooms,median)

kc_house[kc_house$bedrooms == 10, 'bedrooms'] <- 6
kc_house[kc_house$bedrooms == 9 | kc_house$bedrooms == 8, 'bedrooms'] <- 7

tapply(kc_house$price,kc_house$bathrooms,length)
tapply(kc_house$price,kc_house$bathrooms,median)

kc_house[kc_house$bathrooms > 4.25, 'bathrooms'] <- 4.25

# kc_house$bedrooms <- as.factor(kc_house$bedrooms)
# kc_house$bathrooms <- as.factor(kc_house$bathrooms)
# kc_house$floors <- as.factor(kc_house$floors)
# kc_house$condition <- as.factor(kc_house$condition)
# kc_house$view <- as.factor(kc_house$view)
# kc_house$waterfront <- as.factor(kc_house$waterfront)
# kc_house$grade <- as.factor(kc_house$grade)

# No need for id column in this dataset
kc_house$id <-  NULL 

# Sqft_living = sqft_above + sqft_basement, 
# hence, information in sqft_above and sqft_basement are redundant and not needed for analysis.
kc_house$sqft_living15 <- NULL
kc_house$sqft_lot15 <- NULL

# zipcode is directly connected to latitude and longitude, so only zipcode will be fine.
kc_house$lat = NULL
kc_house$long = NULL

# Separate the Date to Year, Month and Day 
# kc_house <- kc_house %>% 
#   mutate(Date=str_replace_all(kc_house$date,"T0{1,}","")) %>% 
#   select(Date,everything(),-date)

# kc_house <- kc_house %>% 
#   mutate(Date=ymd(Date)) %>% 
#   separate(Date,c("year","month","day"))

# kc_house$year <- as.factor(kc_house$year)
# kc_house$month <- as.factor(kc_house$month)
# kc_house$day <- as.factor(kc_house$day)

# Converting Date to numeric for Regression
kc_house$date <- (substr(kc_house$date, 1, 8))
kc_house$date <- ymd(kc_house$date)
kc_house$date <- as.numeric(as.Date(kc_house$date))
kc_house$date <- kc_house$date - 16191

kc_house$renovated <- ifelse(kc_house$yr_renovated == 0, 0, 1)
kc_house$yr_renovated <- NULL

kc_house$zipcode <- as.factor(kc_house$zipcode)

kc_house$sqft_basement <- ifelse(kc_house$sqft_basement > 0, 1, 0)
kc_house$above <- NULL

model <- lm(price ~ . , data = kc_house)
# model <- glm(price ~ . , data = kc_house, family = gaussian())

summary(model)

# The last item in the output is the **p-value**, which tests the fit of the null hypothesis to our data. 
# The null hypothesis assumes that there is no relationship between the independent and dependent variables in the model. 
# The p-value represents the probability you will obtain a result equal to or more extreme than what was actually observed, if the null hypothesis is true. 
# Generally, if the p-value is very low (below 0.05), it meets threshold to reject the null hypothesis. 

# ==== Test and Evaluation ====

# [1] RMSE, [2] R2, [3] MAE
errors <- data.frame() 
best_r2 <- data.frame()
best_mae <- data.frame()
best_rmse <- data.frame()
best_r2_val <- 0
best_mae_val <- 0
best_rmse_val <- 0

iteration_num <- 50

for(i in c(1:iteration_num)){
  sample <- sample.int(n=nrow(kc_house), size = floor(0.70*nrow(kc_house)), replace = F)
  
  # Splitting train and test data
  train <- kc_house[sample, ]
  test  <- kc_house[-sample, ]
  train_model <- lm(price ~ . , data = train)
  
  summary(train_model)
  
  test$pred <- predict(train_model, test)
  act_pred <- data.frame(obs=test$price, pred=test$pred)
  err <- defaultSummary(act_pred)
  err <- as.list(err)
  row_error <- c(0, 0, 0)
  
  for(j in c(1:3)){ # Accumulate the errors
    row_error[j] <- err[j]
  }
  
  # Find best error results among the iterations
  if(i == 1){
    best_r2_val <- as.numeric(err[2])
    best_mae_val <- as.numeric(err[3])
    best_rmse_val <- as.numeric(err[1])
    best_r2 <- test
    best_mae <- test
    best_rmse <- test
  }else{
    
    if(as.numeric(err[1]) < best_rmse_val){
      best_rmse_val <- as.numeric(err[1])
      best_rmse <- test
    }
    if(as.numeric(err[2]) > best_r2_val){
      best_r2_val <- as.numeric(err[2])
      best_r2 <- test
    }
    if(as.numeric(err[3]) < best_mae_val){
      best_mae_val <- as.numeric(err[3])
      best_mae <- test
    }
  }
  
  # Keep error results on every iteration
  errors <- rbind(errors, row_error)
  
}

names(errors) <- c("RMSE", "Rsquared", "MAE")
print(errors)

avg_r2 <- mean(errors$Rsquared)
avg_rmse <- mean(errors$RMSE)
avg_mae <- mean(errors$MAE)

cat("Avarage of Root Mean Squared Error:", avg_rmse, "Avareage of R squared:", avg_r2,
    "Avarage of Mean Absolute Error:", avg_mae)

#Plotting the actual and predicted of the best predictions for the each error types, sometimes they could be same
ggplot(best_rmse,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

ggplot(best_mae,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

ggplot(best_r2,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")


# try the performance of the model by changing the values of the yr_renovated == 0 values with yr_built
# try the performance of the model by adding new column "is_renovated" (could be tried ignoring the yr_renovated)
# date formats needs to be compared (e.g. "yyyymm" vs "yyyymmdd")
# age sold
# year rennovate 0 or 1, 

# https://machinelearningmastery.com/linear-regression-in-r/
# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/