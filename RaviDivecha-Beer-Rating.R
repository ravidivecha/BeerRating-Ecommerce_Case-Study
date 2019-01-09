setwd("C:\\Users\\eravdiv\\OneDrive - Ericsson AB\\Ravi\\PG-Data-Science\\Ecommerce-Rating-Case-Study")

install.packages("recommenderlab") 
library(recommenderlab)
library(dplyr)
library(ggplot2)

# 1. Data preparation
# 1.1 Choose only those beers that have at least N number of reviews

beer_data <- read.csv("beer_data.csv", stringsAsFactors = FALSE )
beer_data <- beer_data[,c(2,1,3)]
beer_data <- beer_data [!duplicated(beer_data[c(1,2)]),]
summary(beer_data)
nrow(beer_data)
# There are in total 474560 number of reviews available.

length(unique(beer_data$beer_beerid))
# There are 40308 number of beers reveiwed by various users.

df_count <- aggregate(review_overall ~ beer_beerid, data = beer_data, FUN = function(x){NROW(x)})
summary(df_count$review_overall)
# > summary(df_count$review_overall)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   11.77    5.00  977.00 
# We see that about 12 number of reviews is the average for various beers.

length(which(df_count$review_overall >= 2))
# 22228 number of beers have atleast 2 reviews.

length(which(df_count$review_overall >= 5))
# 11070 number of beers have atleast 5 reviews.

length(which(df_count$review_overall >= 12))
# 6150 number of beers have atleast 12 reviews which is the average number of reviews.

length(which(df_count$review_overall >= 50))
# 2064 number of beers have atleast 50 reviews which is the average number of reviews.

length(which(df_count$review_overall >= 100))
# 1019 number of beers have atleast 100 reviews which is the average number of reviews.

ggplot(df_count, aes(x="",y=review_overall))+ geom_boxplot()
# Boxplot shows that most number of reviews for beers are above the 3rd quartile.
hist(df_count$review_overall)
# Histogram shows that many beers have received only 1 review.
ggplot(df_count, aes(x = review_overall, y = df_count$beer_beerid, color = "red"))+ geom_point()
# Point graph shows that there are many number of beers which have received low number of reviews.

# Figure out an appropriate value of N using EDA; this may not have one correct answer, but you shouldn't choose beers having extremely low number of ratings

# Based on the above analysis, choosing only those beers which have atleast 50 reviews. High value has been choosen 
# in order to complete UBCF and IBCF evaluation computation. Otherwise 12 would have been a good value with is average number of 
# reviews per beer.

df_count$beer_beerid[which(df_count$review_overall >= 50)]
Beers_N <- df_count$beer_beerid[which(df_count$review_overall >= 50)]
print(paste(" Number of beers identified with atleast 50 number of ratings are :",length(Beers_N)))

beer_data_N <- beer_data[which(beer_data$beer_beerid %in% c(Beers_N)), ]
summary(beer_data_N)
# review_profilename  beer_beerid    review_overall 
# Length:296055      Min.   :    5   Min.   :1.000  
# Class :character   1st Qu.: 1013   1st Qu.:3.500  
# Mode  :character   Median : 3951   Median :4.000  
# Mean   :15398   Mean   :3.882  
# 3rd Qu.:28578   3rd Qu.:4.500  
# Max.   :75086   Max.   :5.000  
print(paste("There are ", nrow(beer_data_N) ," number of rows available for beers with atleast 12 number of reviews per beer"))

# 1.2 Convert this data frame to a "realratingMatrix" before you build your collaborative filtering models
RealRating_BeerData <- as(beer_data_N, "realRatingMatrix")
RealRating_BeerData
# 19493 x 2064 rating matrix of class 'realRatingMatrix' with 296055 ratings.

beers_df <- as(RealRating_BeerData, "data.frame")
str(beers_df)
summary(beers_df)

# user             item            rating     
# BuckeyeNation :   518   2093   :   977   Min.   :1.000  
# mikesgroove   :   505   412    :   967   1st Qu.:3.500  
# BEERchitect   :   460   1904   :   903   Median :4.000  
# northyorksammy:   455   1093   :   840   Mean   :3.882  
# WesWes        :   455   92     :   812   3rd Qu.:4.500  
# TheManiacalOne:   440   4083   :   798   Max.   :5.000  
# (Other)       :293222   (Other):290758    

# 2. Data Exploration

# 2.1 Determine how similar the first ten users are with each other and visualise it
similar_users_beer <- similarity(RealRating_BeerData[1:10, ], method = "cosine", which = "user")
#Similarity matrix
as.matrix(similar_users_beer)

#Visualise similarity matrix
image(as.matrix(similar_users_beer), main = "User similarity")
# We can see that users 1 is similar to 2, 5 and 8.

# 2.2 Compute and visualise the similarity between the first 10 beers

similar_items_beers <- similarity(RealRating_BeerData[,1:10 ], method = "cosine", which = "items")
as.matrix(similar_items_beers)

image(as.matrix(similar_items_beers), main = "Item similarity")
# We find that beer 3 is similar to 5.

# 2.3 What are the unique values of ratings?
unique(beers_df$rating)
# The unique value of ratings are : 4.5 3.5 5.0 4.0 3.0 2.5 1.5 2.0 1.0

# 2.4 Visualise the rating values and notice:

qplot(getRatings(RealRating_BeerData), binwidth = 1, main = "Histogram of ratings", xlab = "Rating")
summary(getRatings(RealRating_BeerData)) 
# We see that the median is to the right of mean. Data is skewed to the right indicating that there are more number of good reviews.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.500   4.000   3.882   4.500   5.000 

qplot(getRatings(normalize(RealRating_BeerData, method = "Z-score")), main = "Histogram of normalized ratings", xlab = "Rating") 
summary(getRatings(normalize(RealRating_BeerData, method = "Z-score"))) # seems better
# We see a binomial distribution but skewed towards the right.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.5747 -0.5383  0.1062  0.0000  0.6879  3.2027 

qplot(rowCounts(RealRating_BeerData), binwidth = 10, 
      main = "Beers Rated on average", 
      xlab = "# of users", 
      ylab = "# of beers rated")
#Most users rate less number of beers. Very few users have rated more beers.

#   The average beer ratings
colMeans(RealRating_BeerData)
hist(colMeans(RealRating_BeerData), breaks=50)
summary(colMeans(RealRating_BeerData))
# we see that most of the ratings are on the higher side. The average is at 3.8
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.426   3.669   3.874   3.818   4.049   4.651 

#   The average user ratings
rowMeans(RealRating_BeerData)
hist(rowMeans(RealRating_BeerData), breaks=50)
summary(rowMeans(RealRating_BeerData))
# we see that most of the ratings are on the higher side. The average is as 3.9.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.714   4.000   3.950   4.300   5.000 

#   The average number of ratings given to the beers
colCounts(RealRating_BeerData)
hist(colCounts(RealRating_BeerData), breaks=50)
summary(colCounts(RealRating_BeerData))
# we see that most users rate on an average 143 beers after N was set as 50.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 50.0    68.0    99.0   143.4   168.0   977.0 

#   The average number of ratings given by the users
rowCounts(RealRating_BeerData)
hist(rowCounts(RealRating_BeerData), breaks=50)
summary(rowCounts(RealRating_BeerData))
# we see that most users give very few number of reviews. The average number is 15 reviews.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    3.00   15.19   10.00  518.00 

# 3. Recommendation Models
# 
# 3.1 Divide your data into training and testing datasets
#   Experiment with 'split' and 'cross-validation' evaluation schemes

# Further subsetting the data based on number of reviews. Setting the number as 50 reviews.
RealRating_BeerData <- RealRating_BeerData[rowCounts(RealRating_BeerData) >= 50]
RealRating_BeerData
# 1578 x 2064 rating matrix of class 'realRatingMatrix' with 179129 ratings.

scheme_split <- evaluationScheme(RealRating_BeerData, method = "split", train = .7, k = 5, given = -1, goodRating = 5)
scheme_cross <- evaluationScheme(RealRating_BeerData, method = "cross-validation", k = 5, given = -1, goodRating = 5)
# 3.2 Build IBCF and UBCF models

scheme_split
# Evaluation scheme using all-but-1 items
# Method: 'split' with 5 run(s).
# Training set proportion: 0.500
# Good ratings: >=5.000000
# Data set: 19493 x 2064 rating matrix of class 'realRatingMatrix' with 179129 ratings.

scheme_cross
# Evaluation scheme using all-but-1 items
# Method: 'cross-validation' with 5 run(s).
# Good ratings: >=5.000000
# Data set: 19493 x 2064 rating matrix of class 'realRatingMatrix' with 179129 ratings.

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score", method="Cosine", nn=50)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)
# removed min rating since it is not recognized.

# 3.3 Compare the performance of the two models and suggest the one that should be deployed
results_split <- evaluate(scheme_split, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_split)

results_cross <- evaluate(scheme_cross, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_cross)


#Plot the ROC curves for UBCF and IBCF and compare them
plot(results_split, annotate = 1:4, legend="topleft")
plot(results_cross, annotate = 1:4, legend="topleft")
# We see that the UBCF curve is much better than the IBCF curve for both split and cross and hence selecting for prediction.

# 3.4 Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"

# 346 is the index for cokes
which(RealRating_BeerData@data@Dimnames[[1]] == "cokes")

# 572 is the index for genog
which(RealRating_BeerData@data@Dimnames[[1]] == "genog")

# 580 is the index for giblet
which(RealRating_BeerData@data@Dimnames[[1]] == "giblet")

## Predicting based on split validation
rec_split <- Recommender(getData(scheme_split, "train"), "UBCF")
rec_split
# Recommender of type 'UBCF' for 'realRatingMatrix' 
# learned using 1104 users.

recom_split_cokes <- predict(rec_split, RealRating_BeerData["cokes",], n = 5)
as(recom_split_cokes, "list")
# $cokes
# [1] "680"   "3158"  "34420" "41815" "79"   

recom_split_genog <- predict(rec_split, RealRating_BeerData["genog",], n = 5)
as(recom_split_genog, "list")
# $genog
# [1] "645"  "3457" "2093" "1160" "4083"

recom_split_giblet <- predict(rec_split, RealRating_BeerData["giblet",], n = 5)
as(recom_split_giblet, "list")
# $giblet
# [1] "7971"  "47658" "16814" "571"   "572"  

## Predicting based on cross validation
rec_cross <- Recommender(getData(scheme_cross, "train"), "UBCF")
rec_cross
# Recommender of type 'UBCF' for 'realRatingMatrix' 
# learned using 1260 users.

recom_cross_cokes <- predict(rec_cross, RealRating_BeerData["cokes",], n=5)
as(recom_cross_cokes, "list")
# $cokes
# [1] "34"    "680"   "1010"  "131"   "34420"

recom_cross_genog <- predict(rec_cross, RealRating_BeerData["genog",], n=5)
as(recom_cross_genog, "list")
# $genog
# [1] "3457" "1005" "645"  "61"   "1093"

recom_cross_giblet <- predict(rec_cross, RealRating_BeerData["giblet",], n=5)
as(recom_cross_giblet, "list")
# $giblet
# [1] "1545"  "29619" "47658" "10672" "1708" 

##################### END OF CODE ####################


