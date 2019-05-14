install.packages("pacman")
library(pacman)
pacman:: p_load(corrplot, ggplot2, dplyr, caret, lubridate, ggthemes, e1071, rsample,
                RColorBrewer, tidyverse, Metrics, randomForest, ranger, pls, h2o)
options(scipen=999)

file_path <- "C://Users//cadob//OneDrive//Masaüstü//kc_house_data.csv"
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

# zipcode is directly connected to latitude and longitude, so only zipcode will be fine.
# kc_house$lat = NULL
# kc_house$long = NULL

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

# Converting Date to numeric for Regression
# kc_house$date <- (substr(kc_house$date, 1, 8))
# kc_house$date <- ymd(kc_house$date)
# kc_house$date <- as.numeric(as.Date(kc_house$date))
# kc_house$date <- kc_house$date - 16191

kc_house$renovated <- ifelse(kc_house$yr_renovated == 0, 0, 1)
kc_house$yr_renovated <- NULL

kc_house$zipcode <- as.factor(kc_house$zipcode)

# Sqft_living = sqft_above + sqft_basement, 
# hence, information in sqft_above and sqft_basement are redundant and not needed for analysis.
kc_house$sqft_basement <- ifelse(kc_house$sqft_basement > 0, 1, 0)
kc_house$sqft_above <- NULL

model <- svm(price ~ . , data = kc_house)
# model <- glm(price ~ . , data = kc_house, family = gaussian())

summary(model)

# The last item in the output is the **p-value**, which tests the fit of the null hypothesis to our data. 
# The null hypothesis assumes that there is no relationship between the independent and dependent variables in the model. 
# The p-value represents the probability you will obtain a result equal to or more extreme than what was actually observed, if the null hypothesis is true. 
# Generally, if the p-value is very low (below 0.05), it meets threshold to reject the null hypothesis. 

# ==== Test and Evaluation ====

#iteration_num <- 10

sample <- sample.int(n=nrow(kc_house), size = floor(0.70*nrow(kc_house)), replace = F)

# Splitting train and test data
train <- kc_house[sample, ]
test  <- kc_house[-sample, ]


#create svr model
train_model <- svm(price ~ . , data = train, kernel = "radial")
summary(train_model)
predictPricesvm <- predict(train_model, test)  ##train model is predicting the test data
error <- test$price - predictPricesvm  ## basic error of the model to  the test data
svrPredictionRMSE <- rmse(test$price,predictPricesvm)  ##rmse of the model to the test data

#now,
#tuning the model, hyperparameter optimization
#The standard way of doing it is by doing a grid search.
#It means we will train a lot of models for the different couples of Ïµ and cost, and choose the best one.

#the tuneResult returns the MSE, don't forget to convert it to RMSE before comparing the value to our previous model.
# we use the tune method to train models with Ïµ=0,0.1,0.2,...,1  
#and cost = 2^2,2^3,2^4,...,2^9 which means it will train  88 models (it can take a long time)


# perform a grid search
# tuneResult <- tune(svm, price ~ . ,  data = train,
#                    ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
# )
# 
# obj <- tune.svm(price~., data = train, 
#                 cost = 2^(2:8), epsilon=seq(0,1,0.1),
                # kernel = "linear") 


##GRID SEARCHING TO TUNING OUR MODEL TO SEE BEST HYPERPARAMETERS FOR BOTH OF 3(RADIAL,POLYNOMIAL,LINEAR) KERNELS.

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
    
    ##cat(i, ".iter ", j, ".jiter ")
    
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
    
    ##print(q)
    #print(err)
    
  }
  
  # add errors to grid
  hyper_grid_for_radial$RMSE[i] <- rmse_total / fold_num
  hyper_grid_for_radial$MAE[i] <- mae_total / fold_num
  hyper_grid_for_radial$Rsquared[i] <- r2_total / fold_num
}

  hyper_grid_for_radial %>% 
  dplyr::arrange(RMSE) %>%
  head(10)

  hyper_grid_for_radial %>% 
  dplyr::arrange(MAE) %>%
  head(10)

  hyper_grid_for_radial %>% 
  dplyr::arrange(desc(Rsquared)) %>%
  head(10)
}
  
if(q==2) { # kernel = polynomial
 
  
  # total number of combinations
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
      
      ##cat(i, ".iter ", j, ".jiter ")
      
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
        gamma           = hyper_grid_for_polynomial$gamma[i]
        
      )
      
      predictions <- predict(model_svm, testData)  ##train model predicting the test data
      
      act_pred <- data.frame(obs=testData$price, pred=predictions)
      err <- defaultSummary(act_pred)
      err <- as.list(err)
      
      rmse_total <- rmse_total + as.numeric(err[1])
      r2_total <- r2_total + as.numeric(err[2])
      mae_total <- mae_total + as.numeric(err[3])
      
      ##print(q)
      #print(err)
      
    }
    
    # add errors to grid
    hyper_grid_for_polynomial$RMSE[i] <- rmse_total / fold_num
    hyper_grid_for_polynomial$MAE[i] <- mae_total / fold_num
    hyper_grid_for_polynomial$Rsquared[i] <- r2_total / fold_num
  }
  
  hyper_grid_for_polynomial %>% 
    dplyr::arrange(RMSE) %>%
    head(10)
  
  hyper_grid_for_polynomial %>% 
    dplyr::arrange(MAE) %>%
    head(10)
  
  hyper_grid_for_polynomial %>% 
    dplyr::arrange(desc(Rsquared)) %>%
    head(10)  
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
        
        ##cat(i, ".iter ", j, ".jiter ")
        
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
        
        ##print(q)
        #print(err)
        
      }
      
      # add errors to grid
      hyper_grid_for_linear$RMSE[i] <- rmse_total / fold_num
      hyper_grid_for_linear$MAE[i] <- mae_total / fold_num
      hyper_grid_for_linear$Rsquared[i] <- r2_total / fold_num
    }
    
    hyper_grid_for_linear %>% 
      dplyr::arrange(RMSE) %>% 
      head(10)
    
    hyper_grid_for_linear %>% 
      dplyr::arrange(MAE) %>%
      head(10)
    
    hyper_grid_for_linear %>%
      dplyr::arrange(desc(Rsquared)) %>%
      head(10)
    
  }
  
}

#The Î³ (gama) has to be tuned to better fit the hyperplane to the data. 
#It is responsible for the linearity degree of the hyperplane, and for that, it is not present when using linear kernels.



cat("Avarage of Root Mean Squared Error:", avg_rmse, "Avareage of R squared:", avg_r2,
    "Avarage of Mean Absolute Error:", avg_mae)

#Plotting the actual and predicted of the best predictions for the each error types, sometimes they could be same
ggplot(best_rmse,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

ggplot(best_mae,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

ggplot(best_r2,aes(x=price,y=pred)) + geom_point() + geom_abline(color="red")

calculate_rmse <- name <- function(true, predicted) {
  res <- true - predicted
  rmse <- sqrt(mean(res^2))
  return(rmse)
}

calculate_rsquare <- function(true, predicted) {
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  
  # For this post, impose floor...
  if (rsq < 0) rsq <- 0
  
  return(rsq)
}


# try the performance of the model by changing the values of the yr_renovated == 0 values with yr_built
# try the performance of the model by adding new column "is_renovated" (could be tried ignoring the yr_renovated)
# date formats needs to be compared (e.g. "yyyymm" vs "yyyymmdd")
# age sold
# year rennovate 0 or 1, 

# https://machinelearningmastery.com/linear-regression-in-r/
# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/