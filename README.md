# House Price Prediction

## Introduction 

Purpose of this project is to implement machine learning algorithms on a real data set Visualization and exploration of the dataset and data preparation are also the important parts of this project. Linear Regression, SVR and Random Forest have been implemented. After, tuning process of the algorithms, performance changings have been analyzed and optimal parameters have been found for the required functions of the algorithms. Performance comparison among these algorithms has been done by using different evaluation metrics. To complete all of those statistical computing and graphics operations, R language has been used.

## Understanding the Data

Name of the dataset that has been used in this project is “House Sales in King County, USA” (https://www.kaggle.com/harlfoxem/housesalesprediction). This dataset contains house sale prices for King County, which includes Seattle. It includes homes sold between May 2014 and May 2015. Thus, aim of this project is to implement machine learning models to predict the price of the houses. Also, there are 21613 rows and 21 columns in this dataset. Description for each attribute explained below:

**ID:** Unique ID for each home sold <br>
**Date:** Date of the home sale <br>
**Price:** Price of each home sold <br>
**Bedrooms:** Number of bedrooms/house <br>
**Bathrooms:**  Number of bathrooms/house <br>
**Sqft_living:** Square footage of the apartments interior living space <br>
**Sqft_lot:** Square footage of the land space <br>
**Floors:** Number of floors <br>
**Waterfront:** A variable for whether the apartment was overlooking the waterfront or not <br>
**View:** An index from 0 to 4 of how good the view of the property was <br>
**Condition:** An index from 1 to 5 on the condition of the apartment <br>
**Grade:** An index from 1 to 13, where 1-3 falls short of building construction and design, 7 has an average level of construction and design, and 11-13 have a high-quality level of construction and design. <br>
**Aqft_above:** The square footage of the interior housing space that is above ground level <br>
**Aqft_basement:** The square footage of the interior housing space that is below ground <br>
**Yr_built:** The year the house was initially built <br>
**Yr_renovated:** The year of the house’s last renovation <br>
**Zipcode:** What zip code area the house is in <br>
**Lat:** Latitude coordinate <br>
**Long:** Longitude coordinate <br>
**Sqft_living15:** Living room area in 2015 <br>
**Sqft_lot15:** Lot size area in 2015 <br>
