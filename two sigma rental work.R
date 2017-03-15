install.packages("syuzhet")
# devtools::install_github("dkahle/ggmap")


library(jsonlite)
library(tidyjson)
library(ggplot2)
library(scales) 
library(magrittr)
library(readr)
library(data.table)
require(Amelia)
library(psych)
library(reshape2)
library(dplyr)
library(purrr)
library(lubridate)
library(sp)
library(rgdal)
library(ggmap)
library(maps)
library(nnet)
library(pROC)
library(MASS)
library(class)
library(syuzhet)

train <- fromJSON("/Users/andrewju/Desktop/Kaggle Two Sigma Rental Listings/train.json") #%>% as.data.frame()
test <- fromJSON("/Users/andrewju/Desktop/Kaggle Two Sigma Rental Listings/test.json")

# samp <- sample(train$bathrooms, 1000)
# samp2 <- lapply(train, function (x) sample(x, 1000))
# # WORKS --> no need to use names(train) instead of just train
# summary(samp2)

vars <- setdiff(names(train), c("photos", "features"))
train_s <- map_at(train, vars, unlist) #%>% tibble::as_tibble(.)
train_s <- tibble::as_tibble(train_s)
summary(head(train_s))
str(head(train_s))

vars_test <- setdiff(names(test), c("photos", "features"))
test_s <- map_at(test, vars, unlist) #%>% tibble::as_tibble(.)
test_s <- tibble::as_tibble(test_s)

# initial data cleanings include changing data types of:
# "created" to date, "address" to geotag or "lat/long", "interest" to factors, "created" to appropriate date-time
# removed "photos" for now b/c unnecessary

train_w <- subset(train_s, select = -c(photos))
train_w$interest_level <- as.factor(train_w$interest_level)
train_w$interest_level <- factor(train_w$interest_level, levels = c("low", "medium", "high"))
train_w$created <- ymd_hms(train$created)

test_w <- subset(test_s, select = -c(photos))
test_w$interest_level <- as.factor(test_w$interest_level)
test_w$interest_level <- factor(test_w$interest_level, levels = c("low", "medium", "high"))
test_w$created <- ymd_hms(test$created)

# for now, also removing some factors like "features" b/c too much, needs feature extraction before useful
train_w <- subset(train_w, select = -c(features,description, building_id, display_address, manager_id, street_address))

test_w <- subset(test_w, select = -c(features,description, building_id, display_address, manager_id, street_address))

plot(train_w$created)

missmap(train_w)
sum(is.na(train_w$bathrooms))
lapply(train_w, function (x) sum(is.na(x)))
# no NAs, complete dataset

# sample data to work with for better manageability in training
# even 10,000 rows seems to be too much for decent training ... 
# maybe only b/c of all the levels, with factors removed 10K seems ok
nrow(train_w)
TR <- lapply(train_w, function(x) sample(x, 10000)) %>% as.data.frame %>% tibble::as_tibble(.)
summary(TR)
length(TR$interest_level)
# imp to set as data.frame too afterwards


# TR <- subset(data = TR, select = -c(created))
# this doesn't work even though does for train_w ... b/c train_w is a tbl & df??? tibble?
str(TR)

str(TR)

# how to do this properly ...
# testTR <- subset(train_w, train_w$listing_id !%in% TR$listing_id))
testTR <- lapply(train_w, function(x) sample(x, 10000)) %>% as.data.frame

summary(TR$interest_level)
summary(testTR$interest_level)
# roughly equivalent ... 

str(testTR)
?subset


# explore data
colnames(train_w)

# logm1 <- lm(data = subset(TR, select = -c(description)), formula = interest_level ~ ., family = binomial)
# wouldn't work with binomial ... but does lm have multinomial function?
# maybe should consider doing ordinal regression instead of multinomial ... 
# multinom would work but would throw away info about the ordering, meaning ordinal could be better tuned
# the predictive categories of low, med, high interest are ordered 
# unlike multinom, which is modeling based on based on outcome of a single category, ordinal bases it on either
# that category OR previous categories based order

str(TR)
?multinom
logm1 <- multinom(data = train_w, formula = interest_level ~ bathrooms + bedrooms + created + price)
summary(logm1)
z_score <- summary(logm1)$coefficients/summary(logm1)$standard.errors
z_score
# why is p (2-tailed z-score 0? and why do I event want this ...)
p <- (1 - pnorm(abs(z_score), 0, 1))*2
p

# summary(train_w$interest_level)
# summary(predict(logm1, test, "class"))
# why are these so off ... make no sense


# fix with the new properly sampled data, etc. 
# fitted.results <- predict(logm1, newdata = testTR, type = "class")
fitted.results <- predict(logm1, newdata = test_w, type = "class")
length(fitted.results)
# fitted.results <- factor(fitted.results, levels(fitted.results)["low", "medium", "high"])
summary(fitted.results)
# why are all fitted result predictions = "low"...?
test_w$logm1pred <- fitted.results
length(TR$bathrooms)
summary(test_w$logm1pred)
summary(test_w)

table(test_w$logm1pred, test_w$interest_level) #%>% plot()
accuracy <- test_w$logm1pred == test_w$interest_level
sum(accuracy)/length(accuracy)
# accuracy is apparently 69% (but should verify calculations again)
# not bad for first model

?multiclass.roc
# !! key !! for any kinda ROC to work, needs to coded as ordered numeric
multiclass.roc(test_w$logm1pred, test_w$interest_level)
# multiclass.roc can't be plotted, just getting AUC
# AUC OF .7255 IS NOT HALF BAD!!!
# but AUC is only measure of performance, not even accuracy actually

test_w$logm1pred <- as.numeric(test_w$logm1pred)
test_w$interest_level <- as.numeric(test_w$interest_level)
levels(test_w$interest_level)

# need to order the factors ...?
# what's with this error here ... 

# new round of features to extract:
# LOCATION/GEOGRAPHY
# get something out of the "features" and "description" variables

# any correlation or use that listing_id and manager_id and building_id could have ...?
# id fields are character, not even numeric ... 
# so correlation doesn't work off the bat 
length(unique(train_s$building_id))/length(train_s$building_id)
# only 15% are unique ... so lots of overlap then
length(unique(train_s$manager_id))/length(train_s$manager_id)
# 7%
length(unique(train_s$listing_id))/length(train_s$listing_id)
# 100% makes sense

# table(train_s$building_id, count(train_s$building_id))
# unnecessary
# table(train_s$building_id) # will do the job, but even better:
uniq_bldg_id <- count(train_s, building_id)
uniq_bldg_id
order_n <- order(uniq_bldg_id$n, decreasing = TRUE)
uniq_bldg_id <- uniq_bldg_id[order_n, ]
uniq_bldg_id
head(uniq_bldg_id, 50)

plot(uniq_bldg_id$n[-1])
# what could bldg_id and such give that address doesn't already though ...? 
# maybe manager_id will give insight into which managers are particularly effective?


str(train_s)

####

# removed latitude and longitude ... they were pretty much all the same ... nope still getting
# Error in svd(X) : infinite or missing values in 'x'
# seems as though it'd have to do with the Hess and scaling required for it ... 
# maybe b/c some variables are 0 ...? remove those?

lapply(TR$bedrooms, function(x) x == 0)
# http://stackoverflow.com/questions/9977686/how-to-remove-rows-with-a-zero-value-in-r 
row_sub <- apply(TR, 1, function(x) all(x != 0))
TR <- TR[row_sub, ]
# even removing all 0s won't change anything ... WTF



?subset

summary(TR$interest_level)
# TR$interest_level2 <- relevel(TR$interest_level, ref = "high")

ordlog1 <- MASS::polr(formula = interest_level ~ bathrooms + bedrooms + price, data = TR, Hess = T)
ordlog1
str(ordlog1)
summary(ordlog1)


any(apply(TR, 1, function(x) all(0)))
# so no 0s ... 
# maybe b/c of "created" being POSIXt? --> ... no even when remove "created" still throws error
str(TR)
summary(TR)
lapply(TR, function(x) all(0))
summary(TR$longitude)


#####
# ?? look into tibbles ... why wouldn't this work with the [] 
train_w[,features]
train_w$features


# possible features to extract / strategy --> 
# map, somehow get geolocation as a feature to work off of ... 
# particular modeling techniques for geospatial?
# extract commonalities from features ... common words, etc.
# would manager_ids matter? or building IDs?
# and then maybe number of pictures would be feature too 
# other than that ... idk

qplot(data = train_w, interest_level, price, color = bedrooms)
# http://petewerner.blogspot.com/2012/12/using-apply-sapply-lapply-in-r.html 

summary(train_s$longitude)
# mapping
# goals: get neighborhood for each location, address?, map all locations 
#
# first need to convert normal lat/lon data into "spatialpointsdataframe" for mapping functions

# ggmap method -- https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf 
# nyc_map <- get_map("new york city")
nyc_map <- qmap("new york city", zoom = 12, color = "bw")
nyc_map
# b/c get_map object is a ggplot object, it's the base layer for normal ggplots
?gglocator
# does gglocator automatically bound based on data...?
# how to get the map zoomed into the right level ...? 
nyc_map + geom_point(aes(x = longitude, y = latitude, color = price), data = train_s) + facet_wrap(~ interest_level)
# clusterfuck, need different viz
# what is an appropriate way of visualizing this ...? somehow binning the average interest or by neighborhood, so not every single location/address

# geting neighorhood & zip code from lon/lat
# train_s$lon_lat <- ()
test1 <- train_s[1,]
test1 <- cbind(test1$longitude, test1$latitude)
str(test1)
length(test1)
# when it's a cbind for an individual obs/row, then it fulfills length = 2 req. 
# but i guess when its for an entire column being assigned to a new column, not the case
?cbind


train_s$postal_code <- rep(NA, times = length(train_s$building_id))
train_s$postal_code <- train_s$postal_code %>% as.factor()
train_s$neighborhood <- rep(NA, times = length(train_s$building_id))
train_s$neighborhood <- train_s$neighborhood %>% as.factor()


revgeocode(test1, output = "more")$neighborhood %>% class() 
# works, but imp to remember it returns a list - makes appending to a dataframe tricky-ish

test1$neighorhood <- revgeocode(test1, output = "more")$neighborhood
# coerces into a list ... 
# returns a list, with different possible factors / levels ... --> how to just get the fucking factor alone
# basically need to change from list to vector(?) --> unlist() finally comes around 
unlist(test1[3]) %>% str()

train_s$lon_lat <- cbind(train_s$longitude, train_s$latitude)
# okay so this makes a list I guess ... weird af
# do I need to apply cbind this way ... ? 
str(train_s$lon_lat)


revgeo_sample <- revgeocode(train_s$lon_lat[1,], output = "more")$neighborhood
### KEY LESSON - ANNOYING, BUT REVGEOCODE ONLY WORKS ONE OBSERVATION AT A TIME
# also ... limit of 2500 per day ... hm fuck

revgeo_sample_data <- train_s[1:50,]
# why does this change the format of lon_lat from double to just one number??

# uncertain whether to make sample entire data, or just the lon.lat col
# just lon.lat makes function work easier but then need to remerge again later

# instinct immediately to write a for-loop ... 
revgeo_sample_data$lon_lat <- cbind(revgeo_sample_data$longitude, revgeo_sample_data$latitude)

revgeo_sample_post <- function(y) {
  # x$neighborhood <- revgeocode(y, output = "more")$neighborhood
  revgeocode(y, output = "more")$postal_code
}

revgeo_sample_neighb <- function(y) {
  revgeocode(y, output = "more")$neighborhood %>% unlist()
  # unlist() addition is key b/c otherwise returns a nested list
}

# sooo ... how to fill in dataframe via loop 
# example: companiesData$sums <- apply(companiesData[,c('revenue', 'profit')], 1, function(x) sum(x))

revgeo_sample(revgeo_sample_data,revgeo_sample_data$lon_lat[1,]) # christ sake it worked

revgeo_sample_data$postal_code <- apply(revgeo_sample_data[, c('lon_lat')], 1, revgeo_sample_post) # wow it works
revgeo_sample_data$neighborhood <- apply(revgeo_sample_data[, c('lon_lat')], 1, revgeo_sample_neighb)
# ok problem now where neighborhood returns a nested list ... was just a matter of putting in unlist() in the function itself i guess?
revgeo_sample_data$neighborhood <- apply(revgeo_sample_data$neighborhood, 1, unlist)
unlist(revgeo_sample_data$neighborhood, recursive=T)
?unlist
revgeo_sample_data$neighborhood %>% summary()
plot(revgeo_sample_data$neighborhood)



# length(train_s$lon_lat2[1])
# train_s$lon_lat2[2]
# typeof(train_s$lon_lat2)

# train_s$lon_lat2 <- NULL

# train_s$lon_lat <- rep(NA, times = length(train_s$building_id))
# train_s$lon_lat <- train_s$lon_lat %>% as.factor()

# train_s$lon_lat <- NULL # reset
lon_lat_make <- function(x) {
  train_s$lon_lat <- cbind(train_s$longitude, train_s$latitude)
}

apply(train_s$lon_lat, 1, lon_lat_make)
?apply

# train_s$postal_code <- revgeocode(train_s$lon_lat, output = "more")$postal_code
# train_s$neighborhood <- revgeocode(train_s$lon_lat, output = "more")$neighborhood
# in the right format but also needs to be numeric & length = 2... 
# so then loop through each one?


?rep

# post_code <- function(x) {
#   train_s$postal_code <- revgeocode(train_s$lon_lat, output = "more")$postal_code
# }

apply(train_s, 1, post_code(train_s$lon_lat))
?apply



########
### OKAY SO NOW METHOD THAT'LL ACTUALLY WORK VS GOOGLE API 2500 LIMIT METHOD ... 
# https://www.kaggle.com/moefasa/two-sigma-connect-rental-listing-inquiries/getting-neighborhoods-from-coordinates/notebook

m_neighborhoods <- c("Chelsea", "Washington Heights", "Harlem", 
                     "East Harlem", "Upper West Side", 
                     "Upper East Side", "Midtown West", "Midtown East",
                     "Greenwich Village",
                     "Lower East Side", "Murray Hill",
                     "Stuyvesant Town", "Upper Manhattan", "Hell's Kitchen", 
                     "East Village", "SoHo", "Financial District", "Gramercy",
                     "Garment District", "Morningside Heights", "Tribeca",
                     "Chinatown", "Times Square")

b_neighborhoods <- c("Bay Ridge", "Sunset Park", "Bensonhurst", "Sheepshead Bay",
                     "Borough Park", "Midwood", "Flatbush", "East Flatbush", 
                     "Park Slope", "East New York", "Bedford-Stuyvesant", 
                     "Williamsburg", "Greenpoint", "Red Hook", "Downtown Brooklyn", 
                     "DUMBO", "Brownsville", "Prospect Park", "Fort Hamilton", 
                     "Cypress Hills", "Bushwick", "Canarsie", "Brooklyn Heights",
                     "Cobble Hill")

q_neighborhoods <- c("Astoria", "Long Island City", "Steinway", "Ridgewood", "Woodside", 
                     "Elmhurst", "Jackson Heights", "Corona", "Murray Hill", "Flushing", 
                     "Kew Gardens", "Fresh Meadows", "Jamaica", "Bayside", "Whitestone")

s_neighborhoods <- c("West New Brighton", "Mariners Harbor")


bx_neighborhoods <- c("West Bronx", "Yankee Stadium")

nj_neighborhoods <- c("Newark")

getCoords <- function(neighborhoods){  
  num_n <- length(neighborhoods)
  if (neighborhoods[1]=="Newark"){
    neighborhoods <- paste0(neighborhoods, ", NJ")
  } else {
    neighborhoods <- paste0(neighborhoods, ", NY")
  }
  
  lat <- rep(0, num_n)
  lon <- rep(0, num_n)
  
  for(i in 1:num_n){
    n <- neighborhoods[i]
    reply <- suppressMessages(geocode(n)) # You may want to expand on this to get status
    lat[i] <- reply$lat
    lon[i] <- reply$lon
  }
  
  return(data.frame(n=neighborhoods, lat=lat, lon=lon))
}

getCoords(nj_neighborhoods)

# how did this return automatic name of rgcdf...

neighb_df <- do.call("rbind", list(getCoords(m_neighborhoods), getCoords(b_neighborhoods), getCoords(bx_neighborhoods), 
                     getCoords(q_neighborhoods), getCoords(s_neighborhoods), getCoords(nj_neighborhoods)))

neighb_df
??knn
neighborhoods_final <- knn(neighb_df[, c("lat", "lon")], train_s[, c("latitude", "longitude")], neighb_df$n, k=1)
train_s$neighborhood <- neighborhoods_final
train_s$neighborhood[1:50]
revgeo_sample_data$neighborhood
# hard to do verification against google maps revgeocode b/c seems as though google maps neighborhoods far more granular
# could always just find list of all the neighborhoods in NYC and throw them into the classified variables and geocode too

cor(train_s$neighborhood, train_s$interest_level)
# Q of how to get corr btwn factors and understand really ... gonna have to go deep into stats/math/prob side soon
table(train_s$neighborhood, train_s$interest_level) #%>% as.data.frame()
?table

######

# trying from the ground_up ... 
coords_pts <- SpatialPoints(train_w[, c("latitude", "longitude")])
spatial_df <- SpatialPointsDataFrame(coords = coords_pts, data = train_w)
proj4string(spatial_df) <- CRS("+proj=longlat +ellps=WGS84")

ggplot() + geom_polygon(data = train_w[1:100,], aes(x = latitude, y = longitude))
# lmao wtf is this
qmap("United States", zoom = 4)

qmplot(longitude, latitude, data = train_w, zoom = 5)
# need to remove that outlier in Africa, and probably focus more on just east coast ... 


str(spatial_df)


getClass("Spatial")
###################
# FEATURE ENGINEERING WITH SENTIMENT ANALYSIS OF DESCRIPTIONS...
train_s$description %>% head()
??syuzhet
?get_nrc_sentiment
sentiment <- get_nrc_sentiment(train_s$description)

colnames(sentiment) <- paste0("sentiment.", colnames(sentiment)) 
sentiment %>% ()
# how to add multiple columns at once to a dataframe... 
train_s[, colnames(sentiment)] <- NA
# now to fill them in ... could do merge, but also could have done  
?merge
# merge(train_s, sentiment, by = colnames(sentiment))
# doesn't do anything b/c merge is by specific thing ... what i want to do is append hm.
train_s[, colnames(sentiment)] # <- NULL # just for reset
train_s <- cbind(train_s, sentiment)
all((train_s[, colnames(sentiment)] == sentiment) == TRUE)
# WORKED ... easy enough with cbind

# okay ... now need to really work on trying out the different models, understanding, ROC, etc. 


train_s[, colnames(sentiment)]
train_s %>% colnames
train_s$sentiment.anger <- sentiment$sentiment.anger
train_s[, colnames(sentiment)] <- lapply(sentiment, )
?cbind

sentiment_cols <- colnames(sentiment)
sentiment_cols
# particular plot is throwing me off b/c trying to get one for each variable, w/o calling each variable ... 
# seems intuitively easy but idk 
sent_sums <- lapply(sentiment, sum)
sent_sums %>% unlist()
sent_sums
sent_plot <- data.frame(colnames(sentiment), "total_sum")
sent_plot$X.total_sum. <- sent_sums
sent_plot %>% colnames()

qplot(sent_plot, x = "colnames.sentiment.", y = "X.total_sum.", geom = "bar")
barplot(sent_plot, x = "colnames.sentiment.", y = "X.total_sum.")
plot(sent_plot)

sent_plot
sentiment$positive %>% max()
train_s$description[58]
# hm actually bored by this now a bit lol




############ initial work 

?sample
# make smaller sample of train for testing purposes
head(train$PassengerId)

head(train)
str(train)
summary(train)
typeof(train)
train_names <- as.list(names(train))
typeof(train_names)

names(train)

unlist_value <- unlist(train$bathrooms, use.names = F)
train_casted <- dcast(names(train$bathrooms), unlist_value)



?melt

train_shaped <- cbindlist(train)
?cbindlist


(names(train$listing_id) == names(train$bedrooms) == names(train$bathrooms))

names(train$bathrooms)

train_shaped <- data.frame(train$listing_id, train$bathrooms, train$bedrooms, train$building_id, train$created, train$description,
                          train$display_address, train$features, train$latitude, train$listing_id, train$longitude, train$manager_id, 
                          train$price, train$street_address, train$interest_level)

cast(train

?unlist
unlist(train)

train_shaped <- data.frame()

length(train$building_id)

train$photos
head(train_shaped)

train_shaped$names.train.listing_id. == names(train$listing_id)
names()
train$listing_id %>% names()


fun (x) <- for (i in x) {
  cbind(train_shaped, x$i)
}



cbind(train_shaped, train$bathrooms)
head(train_shaped)


(x <- c(sort(sample(1:20, 9)), NA))
(y <- c(sort(sample(3:23, 7)), NA))
z <- union(x, y)
z
z <- setdiff(y, x)
z
?setdiff
