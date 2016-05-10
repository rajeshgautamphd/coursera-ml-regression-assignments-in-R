
rm(list = ls())

train_data <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-4-assignment\\kc_house_train_data.csv", colClasses = "character") 
test_data <- read.csv("C:\\Temp\\SharedRW\\coursera-ml-regression\\week-4-assignment\\kc_house_test_data.csv", colClasses = "character") 

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

regression_gradient_descent <- function(feature_matrix, output, initial_weights, step_size, tolerance = 0, l2_penalty = 0, max_iterations=100)
{
    converged <- FALSE
    weights = initial_weights
    iteration <- 0
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
            if(i > 1)
            {#l2_penalty not for incercept term index 0
                fd <- fd + 2*l2_penalty*weights[i]
            }
            # fd <- feature_derivative(errors_,feature_matrix[,i])
            # add the squared derivative to the gradient magnitude
            gradient_sum_squares <- gradient_sum_squares + (fd*fd)
            
            # update the weight based on step size and derivative:
            weights[i] <- weights[i] - step_size*fd
            
        }
        gradient_magnitude = sqrt(gradient_sum_squares)
        if (tolerance > 0 && gradient_magnitude < tolerance)
        {
            converged = TRUE
        }
        if (max_iterations > 0)
        {
            if (iteration > max_iterations)
            {
                converged = TRUE
            }
            iteration <- iteration + 1
        }
    }
    return(weights)
}


#############################################################################
# week-4 
simple_features <- c("sqft_living")
my_output <- "price"

simple_feature_matrix_output = get_numpy_data(train_data, simple_features, my_output)
simple_test_feature_matrix_test_output = get_numpy_data(test_data, simple_features, my_output)

simple_weights_0_penalty <- regression_gradient_descent(simple_feature_matrix_output$features_matrix, simple_feature_matrix_output$output_array,c(0, 0), 1e-12, tolerance = 0, l2_penalty = 0, max_iterations = 1000)


simple_weights_high_penalty <- regression_gradient_descent(simple_feature_matrix_output$features_matrix, simple_feature_matrix_output$output_array,c(0, 0), 1e-12,tolerance = 0, l2_penalty = 1e11, max_iterations = 1000)


errors <- predict_outcome(simple_test_feature_matrix_test_output$features_matrix,c(0, 0)) - simple_test_feature_matrix_test_output$output_array
paste("RSS with 0 weights ", sum((errors) ^ 2))

errors <- predict_outcome(simple_test_feature_matrix_test_output$features_matrix,simple_weights_0_penalty) - simple_test_feature_matrix_test_output$output_array
paste("RSS simple_weights_0_penalty ", sum((errors) ^ 2))

errors <- predict_outcome(simple_test_feature_matrix_test_output$features_matrix,simple_weights_high_penalty) - simple_test_feature_matrix_test_output$output_array
paste("RSS simple_weights_high_penalty ", sum((errors) ^ 2))


#multiple_weights <- function()
{
    multiple_feature_matrix_output = get_numpy_data(train_data, c("sqft_living","sqft_living15"), "price")

    multiple_weights_0_penalty <- regression_gradient_descent(multiple_feature_matrix_output$features_matrix, multiple_feature_matrix_output$output_array,c(0, 0, 0), 1e-12,tolerance = 0, l2_penalty = 0, max_iterations = 1000)
    print(multiple_weights_0_penalty)
    
    multiple_weights_high_penalty <- regression_gradient_descent(multiple_feature_matrix_output$features_matrix, multiple_feature_matrix_output$output_array,c(0, 0, 0), 1e-12,tolerance = 0, l2_penalty = 1e11, max_iterations = 1000)
    print(multiple_weights_high_penalty)
    
    multiple_test_feature_matrix_test_output = get_numpy_data(test_data, c("sqft_living","sqft_living15"), "price")
    
    errors <- predict_outcome(multiple_test_feature_matrix_test_output$features_matrix,c(0, 0, 0)) - multiple_test_feature_matrix_test_output$output_array
    paste("RSS with 0 weights ", sum((errors) ^ 2))
    
    errors <- predict_outcome(multiple_test_feature_matrix_test_output$features_matrix,multiple_weights_0_penalty) - multiple_test_feature_matrix_test_output$output_array
    paste("RSS multiple_weights_0_penalty ", sum((errors) ^ 2))
    
    errors <- predict_outcome(multiple_test_feature_matrix_test_output$features_matrix,multiple_weights_high_penalty) - multiple_test_feature_matrix_test_output$output_array
    paste("RSS multiple_weights_high_penalty ", sum((errors) ^ 2))
    
}

(predict_outcome(multiple_test_feature_matrix_test_output$features_matrix,multiple_weights_0_penalty) - multiple_test_feature_matrix_test_output$output_array)[1]

(predict_outcome(multiple_test_feature_matrix_test_output$features_matrix,multiple_weights_high_penalty) - multiple_test_feature_matrix_test_output$output_array)[1]

