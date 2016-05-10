# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

typify_data <- function (raw_data)
{
    
    raw_data$price <- as.double(raw_data$price)
    raw_data$bedrooms <- as.numeric(raw_data$bedrooms)
    raw_data$bedrooms_squared <- raw_data$bedrooms*raw_data$bedrooms
    
    raw_data$bathrooms <- as.numeric(raw_data$bathrooms)
    raw_data$bed_bath_rooms <- raw_data$bedrooms*raw_data$bathrooms
    
    raw_data$sqft_living <- as.double(raw_data$sqft_living)
    raw_data$log_sqft_living <- log(raw_data$sqft_living)
    
    raw_data$lat <- as.double(raw_data$lat)
    raw_data$long <- as.double(raw_data$long)
    raw_data$lat_long <- raw_data$lat + raw_data$long
    
    return (raw_data)
    
}

polynomial_sframe <- function(feature, degree)
{
    
    poly_sframe <- data.frame(stringsAsFactors=FALSE) 
    
    poly_sframe <- rbind(poly_sframe$power_1,data.frame(feature^1)[1])
    names(poly_sframe) <- c("power_1")
    
    if (degree > 1)
    {
        for(d in 2:(degree+1))
        {
            poly_sframe[,c(paste0("power_",d))] <- data.frame(feature^d)[1]
        }
    }
    
    return (poly_sframe)
}
polynomial_sframe(feature__$v1,2)

sales <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv", colClasses = "character") 
sales <- typify_data(sales)
sales <- sales[order(sales$sqft_living,sales$price),]

poly1_data <- polynomial_sframe(sales$sqft_living,1)
poly1_data$price = sales$price
model1 <- lm( poly1_data$price ~ poly1_data$power_1)
predict1 <- predict(model1,data.frame(poly1_data$sqft_living))

poly2_data <- polynomial_sframe(sales$sqft_living,2)
poly2_data$price = sales$price
model2 <- lm( poly2_data$price ~ poly2_data$power_1 + poly2_data$power_2)
predict2 <- predict(model2,poly2_data[,c("power_1","power_2")])

poly15_data <- polynomial_sframe(sales$sqft_living,15)
poly15_data$price = sales$price
model15 <- get_poly_model(sales$sqft_living,15,poly15_data$price)
predict15 <- predict(model15,poly15_data)

#model15 <- lm( poly15_data$price ~ poly15_data$power_1 + poly15_data$power_2 + poly15_data$power_3 + poly15_data$power_4 + poly15_data$power_5 + poly15_data$power_6 + poly15_data$power_7 + poly15_data$power_8 + poly15_data$power_9 + poly15_data$power_10 + poly15_data$power_11 + poly15_data$power_12 + poly15_data$power_13 + poly15_data$power_14 + poly15_data$power_15)


get_poly_model <- function(feature,degree,output)
{
    polydegree_data <- polynomial_sframe(feature,degree)
    polydegree_data$output = output
    feature_names <- names(polydegree_data)[grepl("power_",names(polydegree_data))]
    #model15 <- lm( polydegree_data$price ~ polydegree_data$power_1 + polydegree_data$power_2 + polydegree_data$power_3 + polydegree_data$power_4 + polydegree_data$power_5 + polydegree_data$power_6 + polydegree_data$power_7 + polydegree_data$power_8 + polydegree_data$power_9 + polydegree_data$power_10 + polydegree_data$power_11 + polydegree_data$power_12 + polydegree_data$power_13 + polydegree_data$power_14 + polydegree_data$power_15)
    
    model <- lm(reformulate(termlabels = feature_names, response = 'output'), data = polydegree_data)
    return(model)
}


library(ggplot2) 
#library(gridExtra)
# http://stackoverflow.com/questions/16509002/ggplot2-two-data-frames-doesnt-know-how-to-deal-with-data-of-class-uneval
#process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_set_1_data.csv", colClasses = "character"),1)

total_data_power_1 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv", colClasses = "character"),1)
total_data_power_2 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv", colClasses = "character"),2)
total_data_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv", colClasses = "character"),15)


ggplot() + geom_point(aes(x = sales$sqft_living, y = sales$price, colour = "data")) + geom_line(aes(x = sales$sqft_living, y = total_data_power_1$prediction, colour="power_1")) + geom_line(aes(x = sales$sqft_living, y = total_data_power_2$prediction, colour = "power_2")) + geom_line(aes(x = sales$sqft_living, y = total_data_power_15$prediction, colour="power_15"))

#C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv

ggplot() + geom_point(aes(x = poly1_data$power_1, y = poly1_data$price, colour = "data")) + geom_line(aes(x = poly1_data$power_1, y = predict1, colour="power_1")) + geom_line(aes(x = poly2_data$power_1, y = predict2, colour = "power_2")) + geom_line(aes(x = poly15_data$power_1, y = predict15, colour="power_15"))

#read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_set_1_data.csv", colClasses = "character")
process_dataset <- function(dataframe, degree)
{
    sales_set <- typify_data(dataframe)
    sales_set <- sales_set[order(sales_set$sqft_living,sales_set$price),]
    
    poly_data <- polynomial_sframe(sales_set$sqft_living,degree)
    poly_data$price = sales_set$price
    model <- get_poly_model(sales_set$sqft_living,degree,poly_data$price)
    predict <- predict(model,poly_data)
    
    list("model" = model, "poly_data" = poly_data, "prediction" = predict)
}



set1_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_set_1_data.csv", colClasses = "character"),15)
set2_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_set_2_data.csv", colClasses = "character"),15)
set3_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_set_3_data.csv", colClasses = "character"),15)
set4_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_set_4_data.csv", colClasses = "character"),15)

ggplot() + geom_point(aes(x = sales$sqft_living, y = sales$price, colour = "data")) + geom_line(aes(x = set1_power_15$poly_data$power_1, y = set1_power_15$prediction, colour="set_1")) + geom_line(aes(x = set2_power_15$poly_data$power_1, y = set2_power_15$prediction, colour="set_1")) + geom_line(aes(x = set3_power_15$poly_data$power_1, y = set3_power_15$prediction, colour="set_3"))

set1_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_train_data.csv", colClasses = "character"),15)
set1_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_valid_data.csv", colClasses = "character"),15)

process_dataset_validation_test <- function(dataframe, degree, validation_set, test_set)
{
    dataframe <- typify_data(dataframe)
    dataframe <- dataframe[order(dataframe$sqft_living,dataframe$price),]
    
    validation_set <- typify_data(validation_set)
    validation_set <- validation_set[order(validation_set$sqft_living,validation_set$price),]
    
    
    process_dataset_info <- process_dataset(dataframe = dataframe, degree = degree)
    validation_poly_data <- polynomial_sframe(validation_set$sqft_living,degree)
    #poly_data$price = validation_set$price
    validation_predict <- predict(process_dataset_info$model,validation_poly_data)
    rss_validation <- sum(((validation_predict - validation_set$price)) ^ 2)
    
    test_set <- typify_data(test_set)
    test_set <- test_set[order(test_set$sqft_living,test_set$price),]
    test_poly_data <- polynomial_sframe(test_set$sqft_living,degree)
    #poly_data$price = validation_set$price
    test_predict <- predict(process_dataset_info$model,test_poly_data)
    rss_test <- sum(((test_predict - test_set$price)) ^ 2)
    
    list("model" = process_dataset_info$model, "vaidation_poly_data" = validation_poly_data, "validation_prediction" = validation_predict, "rss_validation" = rss_validation, "rss_test" = rss_test)
}

train_set <-  read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_train_data.csv", colClasses = "character")
valid_set <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_valid_data.csv", colClasses = "character")
test_set <-  read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_test_data.csv", colClasses = "character")

options(scipen=0)
for(d in 1:15)
{
    
    info_with_validation <- process_dataset_validation_test(train_set,d,valid_set,test_set)
    print(paste(info_with_validation$rss_validation, info_with_validation$rss_test, d))
}

degree <- 04
info_with_validation_10 <- process_dataset_validation_test(train_set,degree,valid_set,test_set)

test_set <- typify_data(test_set)
test_set <- test_set[order(test_set$sqft_living,test_set$price),]

test_set_poly <- polynomial_sframe(test_set$sqft_living,degree)
#poly_data$price = validation_set$price
test_set_predict <- predict(info_with_validation_10$model,test_set_poly)


rss_test <- sum(((test_set_predict - test_set$price)) ^ 2)
rss_test


#########################################################################
# week-4 assignment-1
#########################################################################
#library(penalized)
library(MASS)
get_poly_model_with_l2 <- function(feature,degree,output, l2_penalty)
{
    polydegree_data <- polynomial_sframe(feature,degree)
    polydegree_data$output = output
    feature_names <- names(polydegree_data)[grepl("power_",names(polydegree_data))]
    #model15 <- lm( polydegree_data$price ~ polydegree_data$power_1 + polydegree_data$power_2 + polydegree_data$power_3 + polydegree_data$power_4 + polydegree_data$power_5 + polydegree_data$power_6 + polydegree_data$power_7 + polydegree_data$power_8 + polydegree_data$power_9 + polydegree_data$power_10 + polydegree_data$power_11 + polydegree_data$power_12 + polydegree_data$power_13 + polydegree_data$power_14 + polydegree_data$power_15)
    
    model <- lm.ridge(reformulate(termlabels = feature_names, response = 'output'), data = polydegree_data, lambda = l2_penalty)
    return(model)
}

sales <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv", colClasses = "character") 
sales <- typify_data(sales)
sales <- sales[order(sales$sqft_living,sales$price),]
q1model <- get_poly_model_with_l2(sales$sqft_living,15,sales$price,1.5e-5)

for(salesdatafile in c("wk3_kc_house_set_1_data.csv","wk3_kc_house_set_2_data.csv","wk3_kc_house_set_3_data.csv","wk3_kc_house_set_4_data.csv"))
{
    print(salesdatafile)
    sales <- read.csv(paste0("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\",salesdatafile), colClasses = "character") 
    #sales <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv", colClasses = "character") 
    sales <- typify_data(sales)
    sales <- sales[order(sales$sqft_living,sales$price),c("sqft_living","price")]
    #sales$price <- scale(sales$price)
    #sales <- data.frame(scale(sales))
    
    poly15_data <- polynomial_sframe(sales$sqft_living,15)
    poly15_data$price = sales$price
    model15 <- get_poly_model_with_l2(sales$sqft_living,15,poly15_data$price,1.23e2)
    print(model15$coef)
}

salesshuffled <- read.csv(paste0("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-4-assignment\\",'wk3_kc_house_train_valid_shuffled.csv'), colClasses = "character") 
salesshuffled <- typify_data(salesshuffled)
salesshuffled <- salesshuffled[order(salesshuffled$sqft_living,salesshuffled$price),c("sqft_living","price")]


perform_k_fold_validation <- function(totaldata,degree,l2_penalty,k)
{
    n <- nrow(totaldata)
    rss_test_total <- 0
    for(i in 1:(k-1))
    {
        start = as.integer((n*i)/k)
        end = as.integer((n*(i+1))/k-1)
        #rint (paste(i, start, end))
        
        traindata <- rbind(totaldata[1:start,],totaldata[end+1:n,])
        #traindata$price <- scale(traindata$price)
        #sales <- data.frame(scale(sales))
        
        poly15_data <- polynomial_sframe(traindata$sqft_living,degree)
        poly15_data$price = traindata$price
        trainedmodel <- get_poly_model_with_l2(traindata$sqft_living,degree,poly15_data$price,l2_penalty)
        #print(trainedmodel$coef)
        
        test_set <- totaldata[start:end,]
        #test_set <- typify_data(test_set)
        #test_set <- test_set[order(test_set$sqft_living,test_set$price),]
        
        test_set_poly <- polynomial_sframe(test_set$sqft_living,degree)
        # test_set_predict <- predict.lm(trainedmodel,test_set_poly)
        ones <- data.frame(intercept=rep(1,nrow(test_set_poly)))
        test_set_poly <- cbind(ones,test_set_poly)
        #str(test_set_poly)
        test_set_predict <- as.matrix(test_set_poly) %*% coef(trainedmodel)
        #print(paste("test:",test_set[1,]$price,"predict ", test_set_predict[1]))
        rss_test <- sum(((test_set_predict - test_set$price)) ^ 2)
        #print(paste("i:",i,"rsst_test", rss_test))
        rss_test_total <- (rss_test + rss_test_total)
    }
    
    rss_test_average <- rss_test_total / k
    print(paste("rss_test_average", l2_penalty, rss_test_average))
    return (rss_test_average)
}

for(penaltyexp in seq(1,9,0.5))
{
    penaltytoapply <- 10^penaltyexp
    perform_k_fold_validation(salesshuffled,15,penaltytoapply,10)
}

best: 316.227766017
poly15_data <- polynomial_sframe(salesshuffled$sqft_living,15)
poly15_data$price = salesshuffled$price
model15 <- get_poly_model_with_l2(salesshuffled$sqft_living,15,poly15_data$price,316.227766017)
print(model15$coef)

test_set <-  read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\wk3_kc_house_test_data.csv", colClasses = "character")
test_set <- typify_data(test_set)
test_set <- test_set[order(test_set$sqft_living,test_set$price),c("sqft_living","price")]

test_set_poly <- polynomial_sframe(test_set$sqft_living,15)
# test_set_predict <- predict.lm(trainedmodel,test_set_poly)
ones <- data.frame(intercept=rep(1,nrow(test_set_poly)))
test_set_poly <- cbind(ones,test_set_poly)
#str(test_set_poly)
test_set_predict <- as.matrix(test_set_poly) %*% coef(model15)
#print(paste("test:",test_set[1,]$price,"predict ", test_set_predict[1]))
rss_test <- sum(((test_set_predict - test_set$price)) ^ 2)


#perform_k_fold_validation(salesshuffled,15,1000,10)
