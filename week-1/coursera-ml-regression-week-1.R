


train_data <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-1-assignment\\kc_house_train_data.csv", colClasses = "character") 
test_data <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-1-assignment\\kc_house_test_data.csv", colClasses = "character") 


simple_linear_regression <- function(input_feature, output)
{
    input_feature <- as.double(input_feature)
    output <- as.double(output)
    slope_ <- (sum(output*input_feature) - (sum(output)*sum(input_feature))/as.double(length(input_feature))) / (sum(input_feature*input_feature) - (sum(input_feature)*sum(input_feature))/as.double(length(input_feature)) )
    
    intercept_ <- sum(output)/as.double(length(input_feature)) - slope_*sum(input_feature)/as.double(length(input_feature))
        
    return(list(slope=slope_,intercept=intercept_))
}

get_regression_predictions <- function(input_feature, intercept, slope)
{
    predictions <- (slope*input_feature + intercept)
    return(predictions)
}

sqft_living_to_price <- simple_linear_regression(train_data$sqft_living,train_data$price)

sqft_living_to_price
train_data[1:5,c("sqft_living","price")]

get_regression_predictions(c(2650.0),sqft_living_to_price$intercept,sqft_living_to_price$slope)

get_residual_sum_of_squares <- function(input_feature, output, intercept, slope)
{
    input_feature <- as.double(input_feature)
    output <- as.double(output)
    
    prediction_function <- simple_linear_regression(input_feature, output)
    
    predictions <- get_regression_predictions(input_feature,prediction_function$intercept,prediction_function$slope)
    
    errors <- output - predictions
    
    rss <- errors * errors
    
    return (sum(rss))
}

#sqft_living_to_price <- simple_linear_regression(train_data$sqft_living,train_data$price)

get_residual_sum_of_squares(train_data$sqft_living,train_data$price,sqft_living_to_price$intercept,sqft_living_to_price$slope)

inverse_regression_predictions <- function (output, intercept, slope)
{
    output <- as.double(output)

    #slope*input + intercept = output
    input <- (output - intercept)/slope
    return(input)
}

inverse_regression_predictions(c(800000),sqft_living_to_price$intercept,sqft_living_to_price$slope)

#sqft_living_to_price_test <- simple_linear_regression(test_data$sqft_living,test_data$price)

get_residual_sum_of_squares(test_data$sqft_living,test_data$price,sqft_living_to_price$intercept,sqft_living_to_price$slope)

bedrooms_to_price <- simple_linear_regression(train_data$bedrooms,train_data$price)

get_residual_sum_of_squares(test_data$bedrooms,test_data$price,bedrooms_to_price$intercept,bedrooms_to_price$slope)

