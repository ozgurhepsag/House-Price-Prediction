install.packages("pacman")
library(pacman)
pacman:: p_load(corrplot, ggplot2, dplyr, caret, lubridate, ggthemes, e1071, rsample,
                RColorBrewer, tidyverse, Metrics, randomForest, ranger, pls, class, chemometrics)

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

ggplot(kc_house, aes(x=yr_renovated)) + geom_histogram(fill="blue2") + 
  ggtitle("Number of Renovated Years") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Year Renovated", y="Count")

# Percentage of the yr_renovated = 0
per_yr_renovated <- nrow(kc_house[kc_house$yr_renovated == 0,]) / nrow(kc_house) * 100
cat("Percentage of the yr_renovated = 0 is", per_yr_renovated)

per_view <- nrow(kc_house[kc_house$view == 0,]) / nrow(kc_house) * 100
ggplot(kc_house, aes(x=view)) + geom_histogram(fill="blue2") + 
  ggtitle("Histogram for View") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="View", y="Count")

mean(kc_house[kc_house$view==0, "price"])
mean(kc_house[kc_house$view==1, "price"])
mean(kc_house[kc_house$view==2, "price"])
mean(kc_house[kc_house$view==3, "price"])
mean(kc_house[kc_house$view==4, "price"])

# Percentage of the waterfront = 0
per_waterfront <- nrow(kc_house[kc_house$waterfront == 0,]) / nrow(kc_house) * 100
cat("Percentage of the waterfron = 0 is", per_waterfront)

ggplot(kc_house, aes(waterfront,fill=waterfront)) + geom_histogram(stat="count", fill=c("blue", "green4")) + theme_economist() +
  ggtitle("The Broker Had More Luck in 2014") + theme(plot.title = element_text(hjust = 0.5))

ggplot(kc_house, aes(x=floors)) + geom_histogram(fill="blue2") + 
  ggtitle("Histogram for Floors") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Floors", y="Count")

ggplot(kc_house, aes(x=condition)) + geom_histogram(fill="blue2") + 
  ggtitle("Histogram for Conditions") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Condition", y="Count")

ggplot(kc_house, aes(x=grade)) + geom_histogram(fill="blue2") + 
  ggtitle("Histogram for Grade") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Grade", y="Count")

ggplot(kc_house, aes(x=bedrooms)) + geom_histogram(fill="blue2") + 
  ggtitle("Histogram for Bedrooms") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Bedroom", y="Count")

ggplot(kc_house, aes(x=bathrooms)) + geom_histogram(fill="blue2") + 
  ggtitle("Histogram for Bathroom") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Bathroom", y="Count")

plot(main = "", density(kc_house$price), col = "blue", xlab = "Distribution of the total size of house")

plot(main = "", density(kc_house$sqft_above), col = "blue", xlab = "Distribution of the size of the living rooms")

plot(main = "", density(kc_house$sqft_living15), col = "blue", xlab = "Distribution of the sqft_living15")

plot(main = "", density(kc_house$sqft_living), col = "blue", xlab = "Distribution of the total size of house")

ggplot(kc_house,aes(x=sqft_living))+geom_histogram(binwidth=50,fill="tomato")

ggplot(kc_house,aes(x=bathrooms)) + geom_histogram(fill="green4",binwidth=0.5,size=0.1) +
  scale_x_continuous(limits=c(1,8))

label<-levels(factor(kc_house$zipcode))
mean_price<-rep(0,70)
for( i in 1:length(label)){
  mean_price[i]<-mean(kc_house$price[kc_house$zipcode==label[i]])}
barplot(mean_price,xlab="Zipcode",ylab="Mean price",main="Zipcode vs Meanprice", col="skyblue")

ggplot(kc_house, aes(kc_house$zipcode)) + stat_bin(binwidth=1, colour="black", fill="skyblue") +  labs(x="Zipcode", y="Count")

mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))

kc_house %>% filter(bedrooms<30)%>%
  ggplot(aes(x=bedrooms,y=price,col=bedrooms))+
  geom_point(alpha=0.5,size=2)+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Bedrooms vs Price")+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=bathrooms,y=price,col=bathrooms))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Bedrooms vs Price")+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=grade,y=price,col=grade))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Grade vs Price")+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=view,y=price,col=view))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=View vs Price")+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=floors,y=price,col=floors))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Floors vs Price")+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=view,y=price,col=view))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=View vs Price")+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=yr_renovated,y=price,col=yr_renovated))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Renovated Years vs Price")+theme(legend.position="none")

kc_house %>%
  ggplot(aes(x=waterfront,y=price,col=waterfront))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Waterfront vs Price")+theme(legend.position="none")

kc_house %>% 
  ggplot(aes(x=sqft_living,y=price,col=sqft_living)) + xlab("Size of house")+ ylab("Price") + 
  geom_point() + geom_smooth(method=lm, color = "red") +labs("title=Size of the house vs Price") +theme(legend.position="none")

ggplot(kc_house, aes(yr_built))+geom_histogram(binwidth=5,fill="skyblue",alpha=0.5)+
  scale_x_continuous(limits=c(1900,2016))+ labs(x="Built Year", y="Count")

ggplot(kc_house, aes(yr_built, price)) +
  geom_smooth(se = FALSE, colour = "dodgerblue3") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() +
  theme(text = element_text(face = "bold"))+ labs(x="Built Year", y="Price")

ggplot(data = kc_house, mapping = aes(x = sqft_living, y = price)) + 
  geom_point(colour = 'red') + geom_smooth(method = 'lm')

corr = corrplot::cor(kc_house[,3:21], method = "pearson")
corrplot::corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4"
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
kc_house <- kc_house[-c(8758,15871), ]

tapply(kc_house$price,kc_house$bedrooms,length)
tapply(kc_house$price,kc_house$bedrooms,median)

# kc_house[kc_house$bedrooms == 10, 'bedrooms'] <- 6
# kc_house[kc_house$bedrooms == 9 | kc_house$bedrooms == 8, 'bedrooms'] <- 7

# tapply(kc_house$price,kc_house$bathrooms,length)
# tapply(kc_house$price,kc_house$bathrooms,median)

# kc_house[kc_house$bathrooms > 4.25, 'bathrooms'] <- 4.25

# No need for id column in this dataset
kc_house$id <-  NULL 
kc_house$sqft_living15 <- NULL
kc_house$sqft_lot15 <- NULL

# Separate the Date to Year, Month and Day 
kc_house <- kc_house %>% 
  mutate(Date=str_replace_all(kc_house$date,"T0{1,}","")) %>% 
  select(Date,everything(),-date)

kc_house <- kc_house %>% 
  mutate(Date=ymd(Date)) %>% 
  separate(Date,c("year","month","day"))

kc_house$year <- as.factor(kc_house$year)
kc_house$month <- as.factor(kc_house$month)
kc_house$day <- as.factor(kc_house$day)

kc_house %>% 
  filter(year==2015) %>% 
  ggplot(aes(month,price,fill=month))+geom_histogram(stat="identity")+
  ggtitle("Sales Trend in the Year 2015")+
  theme_economist()

kc_house %>% 
  filter(year==2014) %>% 
  ggplot(aes(month,price,fill=month))+geom_histogram(stat="identity")+
  ggtitle("Sales Trend in the Year 2014")+
  theme_economist()

kc_house$renovated <- ifelse(kc_house$yr_renovated == 0, 0, 1)
kc_house$yr_renovated <- NULL

kc_house$zipcode <- as.factor(kc_house$zipcode)

# Sqft_living = sqft_above + sqft_basement, 
# hence, information in sqft_above and sqft_basement are redundant and not needed for analysis.
kc_house$sqft_basement <- ifelse(kc_house$sqft_basement > 0, 1, 0)
kc_house$sqft_above <- NULL

model <- lm(price ~ . , data = kc_house)
summary(model)

# ==== Test and Evaluation ====
# ==== Linear Regression ====

# [1] RMSE, [2] R2, [3] MAE
errors <- data.frame() 
best_r2 <- data.frame()
best_mae <- data.frame()
best_rmse <- data.frame()
best_r2_val <- 0
best_mae_val <- 0
best_rmse_val <- 0

iteration_num <- 100  

for(i in c(1:iteration_num)){
  
  sample <- sample.int(n=nrow(kc_house), size = floor(0.75*nrow(kc_house)), replace = F)
  
  # Splitting train and test data
  train_lr <- kc_house[sample, ]
  test_lr <- kc_house[-sample, ]
  train_model <- lm(price ~ . , data = train_lr)
  
  summary(train_model)
  
  test_lr$pred <- predict(train_model, test_lr)
  act_pred <- data.frame(obs=test_lr$price, pred=test_lr$pred)
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
    best_r2 <- test_lr
    best_mae <- test_lr
    best_rmse <- test_lr
  }else{
    
    if(as.numeric(err[1]) < best_rmse_val){
      best_rmse_val <- as.numeric(err[1])
      best_rmse <- test_lr
    }
    if(as.numeric(err[2]) > best_r2_val){
      best_r2_val <- as.numeric(err[2])
      best_r2 <- test_lr
    }
    if(as.numeric(err[3]) < best_mae_val){
      best_mae_val <- as.numeric(err[3])
      best_mae <- test_lr
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

# Plotting the actual and predicted of the best predictions for the each error types, sometimes they could be same
ggplot(best_rmse,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

ggplot(best_mae,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

ggplot(best_r2,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

# Plot error results of each iteration
ggplot(errors, aes(x=1:iteration_num, y=Rsquared)) + geom_line() + ggtitle("R Squared Error for House Prices") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(x="Iteration Number")

ggplot(errors, aes(x=1:iteration_num, y=MAE)) + geom_line() + ggtitle("MAE for House Prices") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(x="Iteration Number")

ggplot(errors, aes(x=1:iteration_num, y=RMSE)) + geom_line() + ggtitle("RMSE for House Prices") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(x="Iteration Number")

calculate_rmse <- function(true, predicted) {
  res <- true - predicted
  rmse <- sqrt(mean(res^2))
  return(rmse)
}

calculate_rsquare <- function(true, predicted) {
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  
  # For this post, impose floor...
  if (rsq < 0) 
    rsq <- 0
  
  return(rsq)
}

# ==== Random Forest ====

# One hot encoding for "zipcode" feature. 
# Because, randomForest funtion does not work with the feature that has more than 53 categories.
# temp <- as.data.frame(kc_house$zipcode)
# names(temp) <- "strcol"

# for(level in unique(temp$strcol)){
#   kc_house[paste("is", level, sep = "_")] <- ifelse(temp$strcol == level, 1, 0)
# }

# zipcode <- kc_house$zipcode
# kc_house$zipcode <- NULL

# feature_names <- setdiff(names(kc_house), "price")

# sample_rf <- sample.int(n=nrow(kc_house), size = floor(0.70*nrow(kc_house)), replace = F)
# Splitting train and test data
# train_rf <- kc_house[sample_rf, ]
# test_rf  <- kc_house[-sample_rf, ]

# #rf_model <- ranger(price ~ ., train_rf, num.trees = 700, mtry = floor(length(feature_names) / 3))
# rf_model <- randomForest(price ~ ., train_rf, ntree = 500, mtry = floor(length(feature_names) / 3), importance = T)
# plot(rf_model)

# predictions_rf <- predict(rf_model, test_rf)
# # test_rf$pred <- predictions_rf$predictions
# test_rf$pred <- predictions_rf
# act_pred <- data.frame(obs=test_rf$price, pred=test_rf$pred)
# err <- defaultSummary(act_pred)
# err <- as.list(err)

# print(err)
# summary(rf_model)
# importance(rf_model)
# varImpPlot(rf_model,type=2)

# for grid search
hyper_grid <- expand.grid(
  mtry       = seq(1, 18, by = 1),
  node_size  = seq(2, 9, by = 1),
  sample_size = c(.632, .75, .80),
  OOB_RMSE   = 0,
  OOB_Rsquared = 0
)

# total number of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model_ranger <- ranger(
    formula         = price ~ ., 
    data            = kc_house, 
    num.trees       = 300,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sample_size[i]
  )
  # Add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model_ranger$prediction.error)
  hyper_grid$OOB_Rsquared[i] <- model_ranger$r.squared
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

hyper_grid %>% 
  dplyr::arrange(desc(OOB_Rsquared)) %>%
  head(10)

# New grid search to compare OOB Error and test errors (R2, MAE, RMSE) with cross validation
grid_search <- expand.grid(
  mtry       = seq(1, 18, by = 1),
  node_size  = seq(2, 9, by = 1),
  sample_size = c(.632, .75, .80),
  OOB_RMSE   = 0,
  OOB_Rsquared = 0,
  RMSE       = 0,
  MAE        = 0,
  Rsquared   = 0
)

nrow(grid_search)

fold_num <- 10

for(i in 1:nrow(grid_search)) {
  
  #Randomly shuffle the data
  temp <- kc_house[sample(nrow(kc_house)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(temp)),breaks=fold_num,labels=FALSE)
  
  rmse_total <- 0
  r2_total <- 0 
  mae_total <- 0
  oob_rmse_total <- 0
  oob_rsquared_total <- 0
  
  #Perform 10 fold cross validation
  for(j in 1:fold_num){
    
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==j,arr.ind=TRUE)
    testData <- temp[testIndexes, ]
    trainData <- temp[-testIndexes, ]
    
    # train model
    model_ran <- ranger(
      formula         = price ~ ., 
      data            = trainData, 
      num.trees       = 300,
      mtry            = grid_search$mtry[i],
      min.node.size   = grid_search$node_size[i],
      sample.fraction = grid_search$sample_size[i]
    )
    
    predictions <- predict(model_ran, testData)
    
    act_pred <- data.frame(obs=testData$price, pred=predictions$predictions)
    err <- defaultSummary(act_pred)
    err <- as.list(err)
    
    oob_rmse_total <- oob_rmse_total + sqrt(model_ran$prediction.error)
    oob_rsquared_total <- oob_rsquared_total + model_ran$r.squared
    rmse_total <- rmse_total + as.numeric(err[1])
    r2_total <- r2_total + as.numeric(err[2])
    mae_total <- mae_total + as.numeric(err[3])
  }
  
  # add errors to grid
  grid_search$OOB_RMSE[i] <- oob_rmse_total / fold_num
  grid_search$OOB_Rsquared[i] <- oob_rsquared_total / fold_num
  grid_search$RMSE[i] <- rmse_total / fold_num
  grid_search$MAE[i] <- mae_total / fold_num
  grid_search$Rsquared[i] <- r2_total / fold_num
}

grid_search %>% 
  dplyr::arrange(RMSE) %>%
  head(10)

grid_search %>% 
  dplyr::arrange(desc(Rsquared)) %>%
  head(10)

grid_search %>% 
  dplyr::arrange(MAE) %>%
  head(10)

grid_search %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

grid_search %>% 
  dplyr::arrange(desc(OOB_Rsquared)) %>%
  head(10)

grid_search %>%
  ggplot(aes(x=OOB_RMSE,y=RMSE))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Grade vs Price")+theme(legend.position="none")

grid_search %>%
  ggplot(aes(x=OOB_Rsquared,y=Rsquared))+
  geom_point()+
  geom_smooth(method="lm",se=F, color = "red")+
  labs("title=Grade vs Price")+theme(legend.position="none")

fold_num <- 10
set.seed(1234)
#Randomly shuffle the data
temp <- kc_house[sample(nrow(kc_house)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(temp)),breaks=fold_num,labels=FALSE)

cv_rmse <- numeric(fold_num)
cv_rsquared <- numeric(fold_num)
cv_mae <- numeric(fold_num)

for(j in 1:fold_num){
  
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==j,arr.ind=TRUE)
  testData <- temp[testIndexes, ]
  trainData <- temp[-testIndexes, ]
  
  model_ran <- ranger(
    formula         = price ~ ., data = trainData, num.trees = 300, mtry = 11,
    min.node.size   = 3, sample.fraction = 0.8
  )
  
  predictions <- predict(model_ran, testData)
  act_pred <- data.frame(obs=testData$price, pred=predictions$predictions)
  err <- defaultSummary(act_pred)
  err <- as.list(err)
  
  cv_rmse[j] <- as.numeric(err[1])
  cv_rsquared[j] <- as.numeric(err[2])
  cv_mae[j] <- as.numeric(err[3])
}

cat("Avarage RMSE of the 10-fold cross validation is", mean(cv_rmse), ". Avarage R-squared of the 10-fold cross validation is", 
    mean(cv_rsquared), ". Avarage MAE of the 10-fold cross validation is", mean(cv_mae))

ggplot(as.data.frame(cv_rmse), aes(x=1:fold_num, y=cv_rmse)) + geom_line() + ggtitle("RMSE for 10-Folds") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(x="Fold Number", y="RMSE") + scale_x_discrete(limits=c(1:fold_num))

ggplot(as.data.frame(cv_rsquared), aes(x=1:fold_num, y=cv_rsquared)) + geom_line() + ggtitle("R-squared for 10-Folds") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(x="Fold Number", y="R-squared") + scale_x_discrete(limits=c(1:fold_num))

ggplot(as.data.frame(cv_rmse), aes(x=1:fold_num, y=cv_mae)) + geom_line() + ggtitle("MAE for 10-Folds") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(x="Fold Number", y="MAE") + scale_x_discrete(limits=c(1:fold_num))
# ==== SVR ====

sample <- sample.int(n=nrow(kc_house), size = floor(0.70*nrow(kc_house)), replace = F)

# Splitting train and test data
train <- kc_house[sample, ]
test  <- kc_house[-sample, ]

#create svr model
train_model <- svm(price ~ . , data = train)
summary(train_model)
predictPricesvm <- predict(train_model, test)  ##train model is predicting the test data
error <- test$price - predictPricesvm  ## basic error of the model to  the test data
svrPredictionRMSE <- rmse(test$price,predictPricesvm)  ##rmse of the model to the test data

#now,
#tuning the model / hyperparameter optimization
#The standard way of doing it is by doing a grid search.

#GRID SEARCHING TO TUNING OUR MODEL TO SEE BEST HYPERPARAMETERS FOR BOTH OF 3(RADIAL,POLYNOMIAL,LINEAR) KERNELS.

hyper_grid_for_radial <- expand.grid(
  epsilon    = seq(0, 1, by = 0.1),
  cost       = 2^(2:9),
  gamma      = c(0.008,0.001 ,0.01),
  RMSE       = 0,
  MAE        = 0,
  Rsquared   = 0
  
)

hyper_grid_for_polynomial <- expand.grid(
  epsilon    = seq(0, 1, by = 0.1),
  cost       = 2^(2:9),
  gamma      = c(0.008,0.001 ,0.01),
  degree     = c(2,2.5),
  RMSE       = 0,
  MAE        = 0,
  Rsquared   = 0
)

hyper_grid_for_linear <- expand.grid(
  epsilon    = seq(0, 1, by = 0.1),
  cost       = 2^(2:9),
  #no gamma
  RMSE       = 0,
  MAE        = 0,
  Rsquared   = 0
)

fold_num <- 5  ##

for(q in 1:3) {
  
  if(q==1) { # kernel = radial
    
    # total number of combinations
    nrow(hyper_grid_for_radial)
    
    for(i in 1:nrow(hyper_grid_for_radial)) {
      
      #Randomly shuffle the data
      temp <- kc_house[sample(nrow(kc_house)),]
      
      #Create 5 equally size folds
      folds <- cut(seq(1,nrow(temp)),breaks=fold_num,labels=FALSE)
      
      rmse_total <- 0
      r2_total <- 0
      mae_total <- 0
      
      #Perform 5 fold cross validation
      for(j in 1:fold_num){
        
        #Segement your data by fold using the which() function
        testIndexes <- which(folds==j,arr.ind=TRUE)
        testData <- temp[testIndexes, ]
        trainData <- temp[-testIndexes, ]
        
        # train model
        model_svm <- svm(
          formula         = price ~ .,  ##target
          data            = trainData,
          kernel          = "radial",
          epsilon         = hyper_grid_for_radial$epsilon[i],
          cost            = hyper_grid_for_radial$cost[i],
          gamma           = hyper_grid_for_radial$gamma[i]
        )
        
        predictions <- predict(model_svm, testData)  ##train model predicting the test data
        
        act_pred <- data.frame(obs=testData$price, pred=predictions)
        err <- defaultSummary(act_pred)
        err <- as.list(err)
        
        rmse_total <- rmse_total + as.numeric(err[1])
        r2_total <- r2_total + as.numeric(err[2])
        mae_total <- mae_total + as.numeric(err[3])
      }
      
      # add errors to grid
      hyper_grid_for_radial$RMSE[i] <- rmse_total / fold_num
      hyper_grid_for_radial$MAE[i] <- mae_total / fold_num
      hyper_grid_for_radial$Rsquared[i] <- r2_total / fold_num
    }
  }
  if(q==2) { # kernel = polynomial
    
    #total number of combinations
    nrow(hyper_grid_for_polynomial)
    
    for(i in 1:nrow(hyper_grid_for_polynomial)) {
      
      #Randomly shuffle the data
      temp <- kc_house[sample(nrow(kc_house)),]
      
      #Create 5 equally size folds
      folds <- cut(seq(1,nrow(temp)),breaks=fold_num,labels=FALSE)
      
      rmse_total <- 0
      r2_total <- 0 
      mae_total <- 0
      
      #Perform 5 fold cross validation
      for(j in 1:fold_num){
        
        #Segement your data by fold using the which() function 
        testIndexes <- which(folds==j,arr.ind=TRUE)
        testData <- temp[testIndexes, ]
        trainData <- temp[-testIndexes, ]
        
        # train model
        model_svm <- svm(
          formula         = price ~ .,  ##target
          data            = trainData, 
          kernel          = "polynomial",
          epsilon         = hyper_grid_for_polynomial$epsilon[i],
          cost            = hyper_grid_for_polynomial$cost[i],
          gamma           = hyper_grid_for_polynomial$gamma[i],
          degree          = hyper_grid_for_polynomial$degree[i]
        )
        
        predictions <- predict(model_svm, testData)  ##train model predicting the test data
        
        act_pred <- data.frame(obs=testData$price, pred=predictions)
        err <- defaultSummary(act_pred)
        err <- as.list(err)
        
        rmse_total <- rmse_total + as.numeric(err[1])
        r2_total <- r2_total + as.numeric(err[2])
        mae_total <- mae_total + as.numeric(err[3])
      }
      
      # add errors to grid
      hyper_grid_for_polynomial$RMSE[i] <- rmse_total / fold_num
      hyper_grid_for_polynomial$MAE[i] <- mae_total / fold_num
      hyper_grid_for_polynomial$Rsquared[i] <- r2_total / fold_num
    }
  }
  if(q==3) { ## kernel = linear
    
    # total number of combinations
    nrow(hyper_grid_for_linear)
    
    for(i in 1:nrow(hyper_grid_for_linear)) {
      
      #Randomly shuffle the data
      temp <- kc_house[sample(nrow(kc_house)),]
      
      #Create 5 equally size folds
      folds <- cut(seq(1,nrow(temp)),breaks=fold_num,labels=FALSE)
      
      rmse_total <- 0
      r2_total <- 0
      mae_total <- 0
      
      #Perform 5 fold cross validation
      for(j in 1:fold_num){
        
        #Segement your data by fold using the which() function
        testIndexes <- which(folds==j,arr.ind=TRUE)
        testData <- temp[testIndexes, ]
        trainData <- temp[-testIndexes, ]
        
        # train model
        model_svm <- svm(
          formula         = price ~ .,  ##target
          data            = trainData,
          kernel          = "linear",
          epsilon         = hyper_grid_for_linear$epsilon[i],
          cost            = hyper_grid_for_linear$cost[i]
          #no gamma for linear
        )
        
        predictions <- predict(model_svm, testData)  ##train model predicting the test data
        
        act_pred <- data.frame(obs=testData$price, pred=predictions)
        err <- defaultSummary(act_pred)
        err <- as.list(err)
        
        rmse_total <- rmse_total + as.numeric(err[1])
        r2_total <- r2_total + as.numeric(err[2])
        mae_total <- mae_total + as.numeric(err[3])
      }
      
      # add errors to grid
      hyper_grid_for_linear$RMSE[i] <- rmse_total / fold_num
      hyper_grid_for_linear$MAE[i] <- mae_total / fold_num
      hyper_grid_for_linear$Rsquared[i] <- r2_total / fold_num
    }
  }
}

#list best 10 error types for each kernels.
top10_poly <- hyper_grid_for_polynomial %>% 
  dplyr::arrange(RMSE) %>%
  head(10)

hyper_grid_for_polynomial %>% 
  dplyr::arrange(MAE) %>%
  head(10)

hyper_grid_for_polynomial %>% 
  dplyr::arrange(desc(Rsquared)) %>%
  head(10)  

top10_radial <- hyper_grid_for_radial %>%
  dplyr::arrange(RMSE) %>%
  head(10)

hyper_grid_for_radial %>%
  dplyr::arrange(MAE) %>%
  head(10)

hyper_grid_for_radial %>%
  dplyr::arrange(desc(Rsquared)) %>%
  head(10)

top10_linear <- hyper_grid_for_linear %>%
  dplyr::arrange(RMSE) %>%
  head(10)

hyper_grid_for_linear %>%
  dplyr::arrange(MAE) %>%
  head(10)

hyper_grid_for_linear %>%
  dplyr::arrange(desc(Rsquared)) %>%
  head(10)

RMSE_frame <- data.frame("MAE_Poly"=top10_poly$RMSE,"MAE_Radial" = top10_radial$RMSE,"MAE_Linear" = top10_linear$RMSE)

x1 <- c(1:10)
ggplot() +
  geom_point(data = RMSE_frame, aes(x = x1, y = RMSE_Poly), color="red") +
  geom_point(data = RMSE_frame, aes(x = x1, y = RMSE_Radial), color = "blue") +
  geom_point(data = RMSE_frame, aes(x = x1, y = RMSE_Linear), color = "black") +
  ylab('RMSE Values') +
  xlab('Best 10 instance')+ scale_x_discrete(limits=c(1:10))
