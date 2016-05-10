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
    
    raw_data$sqft_lot <- as.double(raw_data$sqft_lot)
    raw_data$bedrooms <- as.double(raw_data$bedrooms)
    raw_data$floors <- as.double(raw_data$floors)
    raw_data$waterfront <- as.double(raw_data$waterfront)
    raw_data$view <- as.double(raw_data$view)
    raw_data$condition <- as.double(raw_data$condition)
    raw_data$grade <- as.double(raw_data$grade)
    raw_data$sqft_above <- as.double(raw_data$sqft_above)
    raw_data$sqft_basement <- as.double(raw_data$sqft_basement)
    raw_data$yr_built <- as.double(raw_data$yr_built)
    raw_data$yr_renovated <- as.double(raw_data$yr_renovated)
    
    raw_data$sqft_living15 <- as.double(raw_data$sqft_living15)
    raw_data$sqft_lot15 <- as.double(raw_data$sqft_lot15)
    raw_data$zipcode <- as.double(raw_data$zipcode)
    
    raw_data <- raw_data[setdiff(colnames(raw_data), c("id","date"))]
    
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

sales <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\kc_house_data.csv", colClasses = "character") 
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

total_data_power_1 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\kc_house_data.csv", colClasses = "character"),1)
total_data_power_2 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\kc_house_data.csv", colClasses = "character"),2)
total_data_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\kc_house_data.csv", colClasses = "character"),15)


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



set1_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_set_1_data.csv", colClasses = "character"),15)
set2_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_set_2_data.csv", colClasses = "character"),15)
set3_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_set_3_data.csv", colClasses = "character"),15)
set4_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_set_4_data.csv", colClasses = "character"),15)

ggplot() + geom_point(aes(x = sales$sqft_living, y = sales$price, colour = "data")) + geom_line(aes(x = set1_power_15$poly_data$power_1, y = set1_power_15$prediction, colour="set_1")) + geom_line(aes(x = set2_power_15$poly_data$power_1, y = set2_power_15$prediction, colour="set_1")) + geom_line(aes(x = set3_power_15$poly_data$power_1, y = set3_power_15$prediction, colour="set_3"))

set1_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_train_data.csv", colClasses = "character"),15)
set1_power_15 <- process_dataset(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_valid_data.csv", colClasses = "character"),15)

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

train_set <-  read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_train_data.csv", colClasses = "character")
valid_set <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_valid_data.csv", colClasses = "character")
test_set <-  read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_test_data.csv", colClasses = "character")

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

sales <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\kc_house_data.csv", colClasses = "character") 
sales <- typify_data(sales)
sales <- sales[order(sales$sqft_living,sales$price),]
q1model <- get_poly_model_with_l2(sales$sqft_living,15,sales$price,1.5e-5)

for(salesdatafile in c("wk3_kc_house_set_1_data.csv","wk3_kc_house_set_2_data.csv","wk3_kc_house_set_3_data.csv","wk3_kc_house_set_4_data.csv"))
{
    print(salesdatafile)
    sales <- read.csv(paste0("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\",salesdatafile), colClasses = "character") 
    #sales <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-3-assignment\\kc_house_data.csv", colClasses = "character") 
    sales <- typify_data(sales)
    sales <- sales[order(sales$sqft_living,sales$price),c("sqft_living","price")]
    #sales$price <- scale(sales$price)
    #sales <- data.frame(scale(sales,center = TRUE))
    
    poly15_data <- polynomial_sframe(sales$sqft_living,15)
    poly15_data$price = sales$price
    model15 <- get_poly_model_with_l2(sales$sqft_living,15,poly15_data$price,1.23e2)
    print(model15$coef)
}

salesshuffled <- read.csv(paste0("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\",'wk3_kc_house_train_valid_shuffled.csv'), colClasses = "character") 
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

test_set <-  read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_test_data.csv", colClasses = "character")
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


#################################################################################
########### WEEK 5
#################################################################################
require(glmnet)
library(lars)
library(glmnet)

get_features <- function (sales)
{
    sales <- typify_data(sales)
    #sales <- sales[order(sales$sqft_living,sales$price),]
    sales[,'sqft_living_sqrt'] = sqrt(sales[,'sqft_living'])
    sales[,'sqft_lot_sqrt'] = sqrt(sales[,'sqft_lot'])
    sales[,'bedrooms_square'] = sales[,'bedrooms']*sales[,'bedrooms']
    sales[,'floors_square'] = sales[,'floors']*sales[,'floors']
    return(sales)
}

sales <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\kc_house_data.csv", colClasses = "character") )
sales <- data.frame(scale(sales))
#sales <- data.frame(scale(sales,center = TRUE))


all_features <- c('bedrooms', 'bedrooms_square','bathrooms','sqft_living', 'sqft_living_sqrt','sqft_lot', 'sqft_lot_sqrt','floors', 'floors_square','waterfront', 'view', 'condition', 'grade','sqft_above','sqft_basement','yr_built', 'yr_renovated')

## http://stats.stackexchange.com/questions/108529/comparing-ols-ridge-and-lasso
y = sales$price
#X = as.matrix(sales[setdiff(colnames(sales), "price")])
X = as.matrix(sales[all_features])
fit.lasso = glmnet(X, y, alpha = 1, lambda = 5e2)
coef(fit.lasso)
#fit.lasso = glmnet(X, y, alpha = 5e2)
#y_hat_lasso = predict(fit.lasso, X)

plot(fit.lasso,xvar="lambda",label=TRUE)
plot(fit.lasso, plottype="coefficients", label= TRUE)



testing <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_test_data.csv", colClasses = "character") )
training <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_train_data.csv", colClasses = "character") )
validation <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_valid_data.csv", colClasses = "character") )

all_features <- c('bedrooms', 'bedrooms_square','bathrooms','sqft_living', 'sqft_living_sqrt','sqft_lot', 'sqft_lot_sqrt','floors', 'floors_square','waterfront', 'view', 'condition', 'grade','sqft_above','sqft_basement','yr_built', 'yr_renovated')
y_train = training$price
X_train = as.matrix(training[all_features])
X_valid = as.matrix(validation[all_features])
X_test = as.matrix(testing[all_features])

plotdf <- data.frame(penaltyexp=numeric(),rss=numeric())

for(penaltyexp in seq(1,4,0.125))
{
    
    penaltytoapply <- 10^penaltyexp
    fit.lasso = glmnet(X_train, y_train, alpha = 1, lambda = penaltytoapply)
    coef(fit.lasso)
    y_hat_lasso = predict(fit.lasso, X_valid)
#    print(paste(penaltytoapply, sum((validation$price - y_hat_lasso)^2)))
    rss_valid <- sum((validation$price - y_hat_lasso)^2)
    print(paste(rss_valid,penaltytoapply ))
    
    plotdf <- rbind(plotdf,c(penaltytoapply,rss_valid))
}

plot(plotdf,type = 'o')

#  1778.27941  396329461614220

best.fit.lasso = glmnet(X_train, y_train, alpha = 1, lambda = 1778.27941)
coef(best.fit.lasso)

sum((validation$price - predict(best.fit.lasso, X_valid))^2)

sum((testing$price - predict(best.fit.lasso, X_test))^2)


plotdf <- data.frame(penaltyexp=numeric(),rss=numeric())

max_nonzeros <- 7
rangeMin <- 1
for(penaltyexp in seq(1,14,0.25))
{
    penaltytoapply <- 10^penaltyexp
    fit.lasso = glmnet(X_train, y_train, alpha = 1, lambda = penaltytoapply)
    coefficients <- coef(fit.lasso)
    nonzeros <- 0
    for(i in seq(1:dim(coefficients)[1]))
    {
        if (coefficients[i,1] != 0)
        {
            nonzeros <- nonzeros + 1
        }
    }
    print(paste(penaltytoapply, nonzeros ))
    if (nonzeros > max_nonzeros)
    {
        rangeMin <- penaltytoapply
    }
    if (nonzeros < max_nonzeros)
    {
        rangeMax <- penaltytoapply
        break
    }
    plotdf <- rbind(plotdf,c(penaltytoapply,rss_valid))
}

plot(plotdf,type = 'o')


plotdf <- data.frame(penaltyexp=numeric(),rss=numeric())

for(penaltytoapply in seq(7782.7941003892,31622.7766016838,225))
{
    
    #penaltytoapply <- 10^penaltyexp
    fit.lasso = glmnet(X_train, y_train, alpha = 1, lambda = penaltytoapply)
    coefficients <- coef(fit.lasso)
    nonzeros <- 0
    for(i in seq(1:dim(coefficients)[1]))
    {
        if (coefficients[i,1] != 0)
        {
            nonzeros <- nonzeros + 1
        }
    }
    #print(coef(fit.lasso)[1,1])
    y_hat_lasso = predict(fit.lasso, X_valid)
    #    print(paste(penaltytoapply, sum((validation$price - y_hat_lasso)^2)))
    rss_valid <- sum((validation$price - y_hat_lasso)^2)
    print(paste(rss_valid,penaltytoapply, nonzeros ))
    
    plotdf <- rbind(plotdf,c(penaltytoapply,rss_valid))
}

plot(plotdf,type = 'o')

best.fit.lasso = glmnet(X_train, y_train, alpha = 1, lambda = 17782.7941003892)
coef(best.fit.lasso)

###################################
########### WEEK 5 SECOND 
###################################

get_numpy_data <- function (data_sframe, features, output)
{
    
    data_sframe$constant <- 1 # add a constant column to an SFrame
    # prepend variable 'constant' to the features list
    features <- c("constant",features)
    # select the columns of data_SFrame given by the 'features' list into the SFrame 'features_sframe'
    
    # this will convert the features_sframe into a numpy matrix with GraphLab Create >= 1.7!!
    features_matrix_ <- data_sframe[,features]
    # assign the column of data_sframe associated with the target to the variable 'output_sarray'
    
    output_array_ <- data_sframe[,c(output)]
    # this will convert the SArray into a numpy array:
    #output_array = output_sarray.to_numpy() # GraphLab Create>= 1.7!!
    #return(features_matrix, output_array)
    
    return(list(features_matrix=data.matrix(features_matrix_),output_array=output_array_))
    
}

dummy <- get_numpy_data(test_data,c("sqft_living","bedrooms"),"price")

predict_outcome <- function(feature_matrix, weights)
{
    return (feature_matrix %*% weights)
}

normalize_features_junk <- function(features)
{

    #return(data.frame(scale(features,center = TRUE)))
    return(data.frame(scale(features)))
}
normalize_features <- function(features)
{
    norms_ <- (sqrt(colSums(features*features)))
    #names(norms_)  <- names(features)
    normalized_features_ <- sweep(features,2,norms_,'/')
    
    #return (normalized_features_)
    
   return(list(normalized_features=data.matrix(normalized_features_),norms=norms_))
    
}

sales <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\kc_house_data.csv", colClasses = "character") )

dummy <- get_numpy_data(sales,c("sqft_living","bedrooms"),"price")

normalized_features <- normalize_features(dummy$features_matrix)
weights <- rep(0,dim(normalized_features$normalized_features)[2])

weights <- c(1,4,1)

prediction <- predict_outcome(normalized_features$normalized_features, weights)

ro <- rep(0,dim(normalized_features$normalized_features)[2])

output <- dummy$output_array

for(i in 2:length(ro))
{
    ro[i] <- sum(normalized_features$normalized_features[,i]*(output - prediction + weights[i]*normalized_features$normalized_features[,i]))
    print(paste(i,ro[i]))
}
ro[1] <- weights[1]

for(l in c(1.4e8,1.64e8,1.73e8,1.9e8,2.3e8))
{
    print(l)
    print(paste('w1',ro[2] > - l/2 && ro[2] < l/2)) 
    print(paste('w2',ro[3] > - l/2 && ro[3] < l/2)) 
}


lasso_coordinate_descent_step <- function(i, feature_matrix, output, weights, l1_penalty)
{
    # compute prediction
    prediction <- predict_outcome(feature_matrix, weights)    
    # compute ro[i] = SUM[ [feature_i]*(output - prediction + weight[i]*[feature_i]) ]
    #ro <- rep(0,dim(feature_matrix)[2])

    ro_i <- sum(feature_matrix[,i]*(output - prediction + weights[i]*feature_matrix[,i]))
   # print(paste(i,ro[i]))

    if (i == 1)
    {# intercept -- do not regularize
        new_weight_i = ro_i
    }
    else if (ro_i < -l1_penalty/2.)
    {
        new_weight_i = ro_i + l1_penalty/2.
    
    }
    else if (ro_i > l1_penalty/2.)
    {
        new_weight_i = ro_i - l1_penalty/2.
        
    }
    else
    {
        new_weight_i = 0.
    }
        
    return(new_weight_i)
}

#a <- c(c(3./sqrt(13),1./sqrt(10)),c(2./sqrt(13),3./sqrt(10)))
a <- c(c(3./sqrt(13),2./sqrt(13)),c(1./sqrt(10),3./sqrt(10)))
dim(a) <- c(2,2)

lasso_coordinate_descent_step(2, a,c(1., 1.),c(1., 4.), 0.1)

lasso_cyclical_coordinate_descent <- function(feature_matrix, output, initial_weights, l1_penalty, tolerance)
{
    
    weights = initial_weights
    optimized = FALSE
    while(optimized == FALSE)
    {
        
        #weights_diff = []
        weights_diff <- c()
        for (i in 1:(length(weights)))
        {
            #print(i)
            old_weights_i = weights[i] # remember old value of weight[i], as it will be overwritten
    
            # the following line uses new values for weight[0], weight[1], ..., weight[i-1]
            #     and old values for weight[i], ..., weight[d-1]
            weights[i] = lasso_coordinate_descent_step(i, feature_matrix, output, weights, l1_penalty)
            
            # use old_weights_i to compute change in coordinate
            weights_diff <- c(weights_diff,abs(old_weights_i - weights[i]))
            #weights_diff.append(abs(old_weights_i - weights[i]))
        }
        
#        if (max(weights_diff) < tolerance)
        if (max(weights_diff) < tolerance)
        {
            optimized <- TRUE
        }
        #print(paste(max(weights_diff), weights))
        #print(weights)
    }
    
    return (weights)
}

simple_features <- c('sqft_living', 'bedrooms')
my_output <- 'price'
initial_weights = c(0,0,0)
l1_penalty = 1e7
tolerance = 1.0


simple_feature_matrix_output = get_numpy_data(sales, simple_features, my_output)
normalized_simple_feature_matrix_simple_norms = normalize_features(simple_feature_matrix_output$features_matrix) # normalize features

learnt_weights = lasso_cyclical_coordinate_descent(normalized_simple_feature_matrix_simple_norms$normalized_features, simple_feature_matrix_output$output_array,initial_weights, l1_penalty, tolerance)

sum((predict_outcome(normalized_simple_feature_matrix_simple_norms$normalized_features,learnt_weights) - simple_feature_matrix_output$output_array)^2)


#testing <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_test_data.csv", colClasses = "character") )
training <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_train_data.csv", colClasses = "character") )
validation <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_valid_data.csv", colClasses = "character") )

features <- c('bedrooms','bathrooms','sqft_living','sqft_lot','floors','waterfront', 'view', 'condition', 'grade','sqft_above','sqft_basement','yr_built', 'yr_renovated')

training_feature_matrix <- get_numpy_data(training, features, 'price')
training_feature_matrix_normalized = normalize_features(training_feature_matrix$features_matrix) # normalize features

learnt_weights1e7 = lasso_cyclical_coordinate_descent(training_feature_matrix_normalized$normalized_features, training_feature_matrix$output_array,rep(0,length(features) + 1), 1e7, 1.0)
data.frame(c('constant',features),learnt_weights1e7)

learnt_weights1e8 = lasso_cyclical_coordinate_descent(training_feature_matrix_normalized$normalized_features, training_feature_matrix$output_array,rep(0,length(features) + 1), 1e8, 1.0)
data.frame(c('constant',features),learnt_weights1e8)

learnt_weights1e4 = lasso_cyclical_coordinate_descent(training_feature_matrix_normalized$normalized_features, training_feature_matrix$output_array,rep(0,length(features) + 1), 1e4, 5e5)
data.frame(c('constant',features),learnt_weights1e4)

learnt_weights1e7_normalized <- learnt_weights1e7/training_feature_matrix_normalized$norms
learnt_weights1e8_normalized <- learnt_weights1e8/training_feature_matrix_normalized$norms
learnt_weights1e4_normalized <- learnt_weights1e4/training_feature_matrix_normalized$norms

testing <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-5-assignment\\wk3_kc_house_test_data.csv", colClasses = "character") )
testing_feature_matrix <- get_numpy_data(testing, features, 'price')

sum((predict_outcome(testing_feature_matrix$features_matrix,learnt_weights1e7_normalized) - testing_feature_matrix$output_array)^2)
sum((predict_outcome(testing_feature_matrix$features_matrix,learnt_weights1e8_normalized) - testing_feature_matrix$output_array)^2)
sum((predict_outcome(testing_feature_matrix$features_matrix,learnt_weights1e4_normalized) - testing_feature_matrix$output_array)^2)


##########################
######### WEEK 6 #########
##########################


splitdf <- function(dataframe, fraction, seed=NULL) { 
     	if (!is.null(seed)) set.seed(seed) 
     	index <- 1:nrow(dataframe) 
     	trainindex <- sample(index, trunc(length(index)*fraction)) 
     	trainset <- dataframe[trainindex, ] 
     	testset <- dataframe[-trainindex, ] 
     	list(sample=trainset,rest=testset) 
     } 


#sales <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-6-assignment\\kc_house_data_small.csv", colClasses = "character") )
#train_and_validation_test = splitdf(sales,.8, seed=1)
#test <- train_and_validation_test$rest

#train_validation = splitdf(train_and_validation_test$sample,.8, seed=1)
#train <- train_validation$sample
#valid <- train_validation$rest

train <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-6-assignment\\kc_house_data_small_train.csv", colClasses = "character") )
valid <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-6-assignment\\kc_house_data_small_validation.csv", colClasses = "character") )
test <- get_features(read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-6-assignment\\kc_house_data_small_test.csv", colClasses = "character") )


features <- c('bedrooms','bathrooms','sqft_living','sqft_lot','floors','waterfront','view','condition','grade','sqft_above','sqft_basement','yr_built','yr_renovated','lat','long','sqft_living15','sqft_lot15')

training_feature_matrix <- get_numpy_data(train, features, 'price')
valid_feature_matrix <- get_numpy_data(valid, features, 'price')
test_feature_matrix <- get_numpy_data(test, features, 'price')
train_normalized_ = normalize_features(training_feature_matrix$features_matrix) # normalize features
train_normalized = train_normalized_$normalized_features

valid_normalized = sweep(valid_feature_matrix$features_matrix,2,train_normalized_$norms,'/')
test_normalized = sweep(test_feature_matrix$features_matrix,2,train_normalized_$norms,'/')

test_normalized[1,]
train_normalized[10,]

sqrt(sum((test_normalized[1,] - train_normalized[10,])^2))

for(i in seq(1:10))
{
    print(paste(i-1,sqrt(sum((test_normalized[1,] - train_normalized[i,])^2))))
    
}

OneNN <- function (features_instances, features_query)
{
    differences <- c()
    for(i in seq(1:dim(features_instances)[1]))
    {
        diff <- sqrt(sum((features_instances[i,] - features_query)^2))
        differences <- c(differences,diff)
        #print(paste(i,diff))
    }
    
    return(differences)
}

dists <- OneNN(train_normalized,test_normalized[3,])
index <- which.min(dists)
index-1
dists[index]
training_feature_matrix$output_array[index]

k_nearest_neighbors <- function(k, features_instances,features_query)
{
    #features_instances$distance <- sqrt(sum((features_instances - features_query)^2))
    features_instances_df <- data.frame(features_instances)
    features_instances_df$distance <- apply(features_instances_df,1,function(x) sqrt(sum((x - features_query)^2)))
    features_instances_df_sorted <- features_instances_df[order(features_instances_df$distance),]
    indices_ <- match(rownames(features_instances_df_sorted[1:k,]),rownames(features_instances_df))
    #return(features_instances_df_sorted)
    return(list(indices = indices_, distances <- features_instances_df_sorted[1:k,]$distance))
}

k_nearest_neighbors(4,train_normalized,test_normalized[3,])

predict_output_of_query <- function (k, features_train, output_train, features_query)
{
    neighbours <- k_nearest_neighbors(k, features_train,features_query)
    return (mean(output_train[neighbours$indices]))
}

predict_output_of_query(4,train_normalized,training_feature_matrix$output_array,test_normalized[3,])


predict_output <- function(k, features_instances, output_train, features_query)
{
    features_features_query_df <- data.frame(features_query)
    features_features_query_df$prediction <- apply(features_features_query_df,1,function(x) predict_output_of_query(k, features_instances, output_train,x))
    return(features_features_query_df$prediction)
}

predict_output(10,train_normalized,training_feature_matrix$output_array,test_normalized[1:10,])

options( scipen = -10 )
options( digits = 6 )


k_rss <- data.frame(k=numeric(),rss=numeric())
for(k in seq(1:15))
{
    predictions <- predict_output(k,train_normalized,training_feature_matrix$output_array,valid_normalized[,])
    rss <- sum((predictions - valid_feature_matrix$output_array)^2)
    print(paste(k,rss))
    k_rss <- rbind(k_rss,c(k,rss))
}

plot(k_rss,type='o')


