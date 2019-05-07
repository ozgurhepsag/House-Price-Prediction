install.packages("pacman")
library(pacman)
pacman:: p_load(Metrics, car, corrplot, caTools, ggplot2, DAAG, dplyr
                , caret, lubridate, tidyverse, ggthemes, RColorBrewer, reshape2)


options(scipen=999)

file_path <- "C:/Users/ABRA/Desktop/CME4403 Introduction to Machine Learning/Project/kc_house_data.csv"
kc_house <- read.csv(file = file_path, header = TRUE, sep = ",", dec = ".")

# ==== Exploratory Data Analysis ====

# Visualize the data
head(kc_house)
str(kc_house)
dim(kc_house)
summary(kc_house)

# Check for NULL values
apply(kc_house, 2, function(x) sum(is.na(x))) 

hist(kc_house$bedrooms)
boxplot(kc_house$bedrooms)

max(kc_house$bedrooms)

hist(kc_house$bathrooms)
boxplot(kc_house$bathrooms)

hist(kc_house$price)
hist(log(kc_house$price))

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

ggplot(kc_house,aes(x=bathrooms)) + geom_histogram(fill="green4",binwidth=0.5,size=0.1) +
  scale_x_continuous(limits=c(1,8))

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

ggplot(kc_house, aes(yr_built, price)) +
  geom_smooth(se = FALSE, colour = "dodgerblue3") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() +
  theme(text = element_text(face = "bold"))


corr = cor(kc_house[,3:21])
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4"
         , addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7
         , cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))



# ==== Data Preparation ====

price <- kc_house$price
kc_house$price <- log(kc_house$price)

kc_house$id <-  NULL # No need for id column in this dataset

# Sqft_living = sqft_above + sqft_basement, 
# hence, information in sqft_above and sqft_basement are redundant and not needed for analysis.

kc_house$renovated <- ifelse(kc_house$yr_renovated == 0, 0, 1)

# Changing date to yyyymm format
#kc_house$date <- substr(kc_house$date, 1, 6)

# Converting it to numeric as we can only use numeric values for corrleation
#kc_house$date <- as.numeric(as.character(kc_house$date))

# Changing the Date Format for Regression
kc_house$date <- (substr(kc_house$date, 1, 8))
kc_house$date <- ymd(kc_house$date)
kc_house$date <- as.numeric(as.Date(kc_house$date, origin = "2014-04-01"))

kc_house$yr_renovated <- NULL

kc_house$bedrooms <- as.factor(kc_house$bedrooms)
kc_house$bathrooms <- as.factor(kc_house$bathrooms)
kc_house$floors <- as.factor(kc_house$floors)
kc_house$condition <- as.factor(kc_house$condition)
kc_house$view <- as.factor(kc_house$view)
kc_house$waterfront <- as.factor(kc_house$waterfront)
#$waterfront <- NULL
kc_house$zipcode <- as.factor(kc_house$zipcode)
kc_house$renovated <- as.factor(kc_house$renovated)

kc_house$sqft_basement <- ifelse(kc_house$sqft_basement > 0, 1, 0)
kc_house$above <- NULL

#kc_house$sqft_basement <- NULL
# kc_house$date

#kc_house$sqft_lot = NULL
#kc_house$sqft_lot15 = NULL
#kc_house$lat = NULL
#kc_house$long = NULL


model <- lm(price ~ . , data = kc_house)

summary(model)

per_waterfront0 <- nrow(kc_house[kc_house$waterfront == 0,]) / nrow(kc_house[]) * 100 # Percentage of the waterfron = 0

per_view0 <- nrow(kc_house[kc_house$view == 0,]) / nrow(kc_house[]) * 100 # Percentage of the waterfron = 0


# try the performance of the model by changing the values of the yr_renovated == 0 values with yr_built
# try the performance of the model by adding new column "is_renovated" (could be tried ignoring the yr_renovated)
# date formats needs to be compared (e.g. "yyyymm" vs "yyyymmdd")
# age sold
# year rennovate 0 or 1, 
# factor

# https://machinelearningmastery.com/linear-regression-in-r/
# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/