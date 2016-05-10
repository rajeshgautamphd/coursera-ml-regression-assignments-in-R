
rm(list = ls())

train_data <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-2-assignment\\kc_house_train_data.csv", colClasses = "character") 
test_data <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-2-assignment\\kc_house_test_data.csv", colClasses = "character") 

train_data$price <- as.double(train_data$price)
train_data$bedrooms <- as.numeric(train_data$bedrooms)
train_data$bedrooms_squared <- train_data$bedrooms*train_data$bedrooms

train_data$bathrooms <- as.numeric(train_data$bathrooms)
train_data$bed_bath_rooms <- train_data$bedrooms*train_data$bathrooms

train_data$sqft_living <- as.double(train_data$sqft_living)
train_data$log_sqft_living <- log(train_data$sqft_living)

train_data$lat <- as.double(train_data$lat)
train_data$long <- as.double(train_data$long)
train_data$lat_long <- train_data$lat + train_data$long


test_data$price <- as.double(test_data$price)
test_data$bedrooms <- as.numeric(test_data$bedrooms)
test_data$bedrooms_squared <- test_data$bedrooms*test_data$bedrooms

test_data$bathrooms <- as.numeric(test_data$bathrooms)
test_data$bed_bath_rooms <- test_data$bedrooms*test_data$bathrooms

test_data$sqft_living <- as.double(test_data$sqft_living)
test_data$log_sqft_living <- log(test_data$sqft_living)

test_data$lat <- as.double(test_data$lat)
test_data$long <- as.double(test_data$long)
test_data$lat_long <- test_data$lat + test_data$long


mean(test_data$bedrooms_squared)
mean(test_data$bed_bath_rooms)
mean(test_data$log_sqft_living)
mean(test_data$lat_long)


train_data_price <- train_data$price
train_data_sqft_living  <- train_data$sqft_living
train_data_bedrooms <- train_data$bedrooms
train_data_bathrooms <- train_data$bathrooms
train_data_lat <- train_data$lat
train_data_long <- train_data$long
train_data_bed_bath_rooms <- train_data$bed_bath_rooms
train_data_bedrooms_squared <- train_data$bedrooms_squared
train_data_log_sqft_living <- train_data$log_sqft_living
train_data_lat_long <- train_data$lat_long


model1 <- lm( train_data_price ~ train_data_sqft_living  + train_data_bedrooms + train_data_bathrooms + train_data_lat + train_data_long)
sum(resid(model1) ^ 2)
model2 <- lm( train_data_price ~ train_data_sqft_living  + train_data_bedrooms + train_data_bathrooms + train_data_lat + train_data_long + train_data_bed_bath_rooms)
sum(resid(model2) ^ 2)
model3 <- lm( train_data_price ~ train_data_sqft_living  + train_data_bedrooms + train_data_bathrooms + train_data_lat + train_data_long + train_data_bed_bath_rooms + train_data_bedrooms_squared + train_data_log_sqft_living + train_data_lat_long)
sum(resid(model3) ^ 2)

#http://www.montefiore.ulg.ac.be/~kvansteen/GBIO0009-1/ac20092010/Class8/Using%20R%20for%20linear%20regression.pdf

predict1 <- predict(model1,data.frame(train_data_sqft_living = test_data$sqft_living, train_data_bedrooms = test_data$bedrooms, train_data_bathrooms = test_data$bathrooms, train_data_lat = test_data$lat , train_data_long = test_data$long, train_data_bed_bath_rooms = test_data$bed_bath_rooms, train_data_bedrooms_squared = test_data$bedrooms_squared,  train_data_log_sqft_living = test_data$log_sqft_living,  train_data_lat_long = test_data$lat_long))
sum((predict1 - test_data$price) ^ 2)

predict2 <- predict(model2,data.frame(train_data_sqft_living = test_data$sqft_living, train_data_bedrooms = test_data$bedrooms, train_data_bathrooms = test_data$bathrooms, train_data_lat = test_data$lat , train_data_long = test_data$long, train_data_bed_bath_rooms = test_data$bed_bath_rooms, train_data_bedrooms_squared = test_data$bedrooms_squared,  train_data_log_sqft_living = test_data$log_sqft_living,  train_data_lat_long = test_data$lat_long))
sum((predict2 - test_data$price) ^ 2)

predict3 <- predict(model3,data.frame(train_data_sqft_living = test_data$sqft_living, train_data_bedrooms = test_data$bedrooms, train_data_bathrooms = test_data$bathrooms, train_data_lat = test_data$lat , train_data_long = test_data$long, train_data_bed_bath_rooms = test_data$bed_bath_rooms, train_data_bedrooms_squared = test_data$bedrooms_squared,  train_data_log_sqft_living = test_data$log_sqft_living,  train_data_lat_long = test_data$lat_long))
sum((predict3 - test_data$price) ^ 2)


model4 <- lm( train_data_price ~ train_data_sqft_living)
sum(resid(model4) ^ 2)
predict4 <- predict(model4,data.frame(train_data_sqft_living = test_data$sqft_living, train_data_bedrooms = test_data$bedrooms, train_data_bathrooms = test_data$bathrooms, train_data_lat = test_data$lat , train_data_long = test_data$long, train_data_bed_bath_rooms = test_data$bed_bath_rooms, train_data_bedrooms_squared = test_data$bedrooms_squared,  train_data_log_sqft_living = test_data$log_sqft_living,  train_data_lat_long = test_data$lat_long))
sum((predict4 - test_data$price) ^ 2)






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

feature_derivative <- function(errors, features)
{
 return (-2*( features %*% errors))
}

regression_gradient_descent <- function(feature_matrix, output, initial_weights, step_size, tolerance)
{
    converged <- FALSE
    weights = initial_weights
    #weights = np.array(initial_weights)
    while(!converged)
    {
        gradient_sum_squares = 0
        # compute the predictions based on feature_matrix and weights:
        # compute the errors as predictions - output:
        errors_ <- output - predict_outcome(feature_matrix, weights)
        
    # while not converged, update each weight individually:
        for(i in 1:length(weights))
        {
                
            # Recall that feature_matrix[:, i] is the feature column associated with weights[i]
            # compute the derivative for weight[i]:
            
            fd <- feature_derivative(errors_,feature_matrix[,i])
            # add the squared derivative to the gradient magnitude
            gradient_sum_squares <- gradient_sum_squares + (fd*fd)
            
            # update the weight based on step size and derivative:
            weights[i] <- weights[i] - step_size*fd
            
        }
        gradient_magnitude = sqrt(gradient_sum_squares)
        if (gradient_magnitude < tolerance)
        {
            converged = TRUE
        }
    }

    return(weights)
}

simple_features <- c("sqft_living")
my_output <- "price"

simple_feature_matrix_output <- get_numpy_data(train_data, simple_features, my_output)
initial_weights = c(-47000., 1.)
step_size = 7e-12
tolerance = 2.5e7

simple_weights <- regression_gradient_descent(simple_feature_matrix_output$features_matrix, simple_feature_matrix_output$output_array,initial_weights, step_size,tolerance)

predict_outcome(get_numpy_data(test_data[1,], simple_features, my_output)$features_matrix,simple_weights)

errors <- predict_outcome(get_numpy_data(test_data, simple_features, my_output)$features_matrix,simple_weights) - get_numpy_data(test_data, simple_features, my_output)$output_array

rss_simple_model_0 <- sum((errors) ^ 2)

#================================================================================
model_features_ <- c("sqft_living","sqft_living15")
my_output <- "price"

model_feature_matrix_output <-get_numpy_data(data_sframe = train_data, model_features_, output = my_output)
#model_feature_matrix_output <- get_numpy_data(train_data, ,model_features, my_output)
initial_weights = c(-100000.0, 1., 1.)
step_size = 4e-12
tolerance = 1e9

model_weights <- regression_gradient_descent(model_feature_matrix_output$features_matrix, model_feature_matrix_output$output_array,initial_weights, step_size,tolerance)

predict_outcome(get_numpy_data(test_data[1,], model_features_, my_output)$features_matrix,model_weights)


errors <- predict_outcome(get_numpy_data(test_data, model_features_, my_output)$features_matrix,model_weights) - get_numpy_data(test_data, model_features_, my_output)$output_array

rss_model_features <- sum((errors) ^ 2)

